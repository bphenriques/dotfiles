# Implements secret generation for services.
# Consumes services.*.secrets defined in services-registry.nix.
#
# Generates secrets on first boot, stores in /var/lib/homelab-secrets/<service>/<file>.
# Creates per-service Unix groups for isolation, auto-wires systemd dependencies.
#
# Rotation: sudo rm /var/lib/homelab-secrets/<service>/<file> && sudo systemctl restart homelab-secrets-<service>
{ lib, config, pkgs, ... }:
let
  cfg = config.custom.homelab;

  # Extract services that have secrets defined
  serviceSecrets = lib.filterAttrs (_: svc: svc.secrets.files != { }) cfg.services;

  # Helper to generate the shell script for a service's secrets
  mkGeneratorScript = serviceName: svc: let
    secretsCfg = svc.secrets;
    filesList = lib.mapAttrsToList (name: file: {
      inherit name;
      inherit (file) bytes path;
    }) secretsCfg.files;
    envVars = lib.mapAttrsToList (envVar: secretName: {
      inherit envVar;
      secretPath = secretsCfg.files.${secretName}.path;
    }) secretsCfg.envFile;
    hasEnvFile = secretsCfg.envFile != { };
  in pkgs.writeShellScript "generate-secrets-${serviceName}" ''
    set -euo pipefail

    secretsDir="${secretsCfg.secretsDir}"
    group="${secretsCfg.group}"

    mkdir -p "$secretsDir"
    chown root:"$group" "$secretsDir"
    chmod 750 "$secretsDir"

    ${lib.concatMapStringsSep "\n" (file: ''
      if [ ! -f "${file.path}" ]; then
        echo "Generating secret: ${file.name}"
        ${pkgs.openssl}/bin/openssl rand -hex ${toString file.bytes} > "${file.path}"
        chown root:"$group" "${file.path}"
        chmod 640 "${file.path}"
      fi
    '') filesList}

    ${lib.optionalString hasEnvFile ''
      # Generate environment file from secrets
      envFile="${secretsCfg.envFilePath}"
      echo "Generating env file: $envFile"
      cat > "$envFile" <<EOF
      ${lib.concatMapStringsSep "\n" (v: "${v.envVar}=$(cat \"${v.secretPath}\")") envVars}
      EOF
      chown root:"$group" "$envFile"
      chmod 640 "$envFile"
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
