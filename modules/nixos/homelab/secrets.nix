# Implements secret generation and templating for services.
# Consumes services.*.secrets defined in services-registry.nix.
#
# Generates secrets on first boot, stores in /var/lib/homelab-secrets/<service>/<file>.
# Templates are rendered using replace-secret (similar to sops-nix).
# Creates per-service Unix groups for isolation, auto-wires systemd dependencies.
#
# Rotation: sudo rm /var/lib/homelab-secrets/<service>/<file> && sudo systemctl restart homelab-secrets-<service>
{ lib, config, pkgs, ... }:
let
  cfg = config.custom.homelab;

  # Extract services that have secrets or templates defined
  serviceSecrets = lib.filterAttrs (_: svc: 
    svc.secrets.files != { } || svc.secrets.templates != { }
  ) cfg.services;

  # Placeholder format (must match _secrets-schema.nix)
  mkPlaceholder = serviceName: secretName: "__HOMELAB_SECRET_${serviceName}_${secretName}__";

  # Helper to generate the shell script for a service's secrets
  mkGeneratorScript = serviceName: svc: let
    secretsCfg = svc.secrets;
    
    filesList = lib.mapAttrsToList (name: file: {
      inherit name;
      inherit (file) bytes path;
    }) secretsCfg.files;
    
    templatesList = lib.mapAttrsToList (name: tmpl: {
      inherit name;
      inherit (tmpl) path;
      placeholder = mkPlaceholder serviceName;
      srcPath = pkgs.writeText "homelab-template-${serviceName}-${name}" tmpl.content; # Content in nix store (safe - only placeholders, no secrets)
    }) secretsCfg.templates;
    
    hasTemplates = templatesList != [ ];
  in pkgs.writeShellScript "generate-secrets-${serviceName}" ''
    set -euo pipefail

    secretsDir="${secretsCfg.secretsDir}"
    group="${secretsCfg.group}"

    mkdir -p "$secretsDir"
    chown root:"$group" "$secretsDir"
    chmod 750 "$secretsDir"

    # Generate raw secret files (only if missing)
    ${lib.concatMapStringsSep "\n" (file: ''
      if [ ! -f "${file.path}" ]; then
        echo "Generating secret: ${file.name}"
        ${pkgs.openssl}/bin/openssl rand -hex ${toString file.bytes} > "${file.path}"
        chown root:"$group" "${file.path}"
        chmod 640 "${file.path}"
      fi
    '') filesList}

    # Render templates (always re-rendered to pick up rotated secrets)
    ${lib.optionalString hasTemplates ''
      ${lib.concatMapStringsSep "\n" (tmpl: ''
        echo "Rendering template: ${tmpl.name}"
        install -m 640 -o root -g "$group" "${tmpl.srcPath}" "${tmpl.path}"
        ${lib.concatMapStringsSep "\n" (file: ''
          ${pkgs.replace-secret}/bin/replace-secret \
            '${mkPlaceholder serviceName file.name}' \
            '${file.path}' \
            '${tmpl.path}'
        '') filesList}
      '') templatesList}
    ''}

    echo "All secrets for ${serviceName} are ready"
  '';

  # Collision detection for explicit GIDs
  explicitGids = lib.filter (g: g != null) (lib.mapAttrsToList (_: svc: svc.secrets.gid) serviceSecrets);
  dupGids = lib.filter (gid: lib.count (g: g == gid) explicitGids > 1) (lib.unique explicitGids);
in {
  config = lib.mkIf (serviceSecrets != { }) {
    assertions = [{
      assertion = dupGids == [];
      message = "Service secrets have duplicate explicit gids: ${toString dupGids}";
    }];

    # Create groups for each service
    users.groups = lib.mapAttrs' (_: svc:
      lib.nameValuePair svc.secrets.group (lib.optionalAttrs (svc.secrets.gid != null) {
        gid = svc.secrets.gid;
      })
    ) serviceSecrets;

    # Create generator services and wire dependencies
    systemd.services = lib.mkMerge [
      # Generator services (one per service with secrets)
      (lib.mapAttrs' (name: svc:
        lib.nameValuePair "homelab-secrets-${name}" {
          description = "Generate secrets for ${name}";
          wantedBy = [ "multi-user.target" ];
          before = map (s: "${s}.service") svc.secrets.systemd.dependentServices;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = mkGeneratorScript name svc;
          };
        }
      ) serviceSecrets)

      # Wire dependencies into dependent services
      (lib.mkMerge (lib.mapAttrsToList (name: svc:
        lib.listToAttrs (map (depSvc: {
          name = depSvc;
          value = {
            requires = [ "homelab-secrets-${name}.service" ];
            after = [ "homelab-secrets-${name}.service" ];
            partOf = [ "homelab-secrets-${name}.service" ];
            serviceConfig.SupplementaryGroups = [ svc.secrets.group ];
          };
        }) svc.secrets.systemd.dependentServices)
      ) serviceSecrets))
    ];

    # Warnings for non-rotatable secrets
    warnings = lib.flatten (lib.mapAttrsToList (serviceName: svc:
      lib.mapAttrsToList (fileName: file:
        lib.optional (!file.rotatable)
          "Secret '${serviceName}/${fileName}' is non-rotatable. Deleting it will regenerate, which may desync external systems."
      ) svc.secrets.files
    ) serviceSecrets);
  };
}
