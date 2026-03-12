# Implements secret generation and templating for services and standalone owners.
# Consumes services.*.secrets and custom.homelab.secrets defined in services-registry.nix.
#
# Generates secrets on first boot, stores in /var/lib/homelab-secrets/<owner>/<file>.
# Templates are rendered using replace-secret with a global substitution map:
# - All secret file placeholders across all owners
# - All OIDC credential placeholders (id + secret)
# Cross-owner and OIDC dependencies are auto-detected from placeholder patterns in template content.
#
# Creates per-owner Unix groups for isolation, auto-wires systemd dependencies.
#
# Rotation: sudo rm /var/lib/homelab-secrets/<owner>/<file> && sudo systemctl restart homelab-secrets-<owner>
{ lib, config, pkgs, ... }:
let
  cfg = config.custom.homelab;

  # Collect secret owners from both services and standalone secrets
  serviceOwners = lib.mapAttrs (_: svc: svc.secrets) (lib.filterAttrs (_: svc:
    svc.secrets.files != { } || svc.secrets.templates != { }
  ) cfg.services);

  standaloneOwners = lib.filterAttrs (_: owner:
    owner.files != { } || owner.templates != { }
  ) cfg.secrets;

  allOwners = serviceOwners // standaloneOwners;

  # Placeholder format (must match _secrets-schema.nix)
  mkPlaceholder = ownerName: secretName: "__HOMELAB_SECRET_${ownerName}_${secretName}__";

  # Global substitution map: all secret file placeholders across all owners
  globalSecretVars = lib.foldlAttrs (acc: ownerName: ownerCfg:
    acc // lib.mapAttrs' (fileName: file:
      lib.nameValuePair (mkPlaceholder ownerName fileName) file.path
    ) ownerCfg.files
  ) { } allOwners;

  # OIDC credential placeholders (from services with OIDC enabled)
  oidcVars = lib.foldlAttrs (acc: _: svc:
    if svc.oidc.enable then acc // {
      ${svc.oidc.id.placeholder} = svc.oidc.id.file;
      ${svc.oidc.secret.placeholder} = svc.oidc.secret.file;
    } else acc
  ) { } cfg.services;

  allVars = globalSecretVars // oidcVars;

  # Auto-detect cross-owner dependencies from placeholder patterns in template content
  crossDeps = ownerName: ownerCfg:
    lib.filter (otherName: otherName != ownerName &&
      lib.any (tmpl: lib.hasInfix "__HOMELAB_SECRET_${otherName}_" tmpl.content)
        (lib.attrValues ownerCfg.templates)
    ) (lib.attrNames allOwners);

  # Auto-detect OIDC dependency from placeholder patterns in template content
  needsOidc = ownerCfg:
    lib.any (tmpl: lib.hasInfix "@HOMELAB_OIDC_" tmpl.content)
      (lib.attrValues ownerCfg.templates);

  # Helper to generate the shell script for an owner's secrets
  mkGeneratorScript = ownerName: ownerCfg: let
    filesList = lib.mapAttrsToList (name: file: {
      inherit name;
      inherit (file) bytes path;
    }) ownerCfg.files;

    templatesList = lib.mapAttrsToList (name: tmpl: {
      inherit name;
      inherit (tmpl) path;
      srcPath = pkgs.writeText "homelab-template-${ownerName}-${name}" tmpl.content;
    }) ownerCfg.templates;

    hasTemplates = templatesList != [ ];
  in pkgs.writeShellScript "generate-secrets-${ownerName}" ''
    set -euo pipefail

    secretsDir="${ownerCfg.secretsDir}"
    group="${ownerCfg.group}"

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
        mkdir -p "$(dirname '${tmpl.path}')"
        install -m 640 -o root -g "$group" "${tmpl.srcPath}" "${tmpl.path}"
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (placeholder: filePath: ''
          ${pkgs.replace-secret}/bin/replace-secret \
            '${placeholder}' \
            '${filePath}' \
            '${tmpl.path}'
        '') allVars)}
        if ${pkgs.gnugrep}/bin/grep -qE '__HOMELAB_SECRET_|@HOMELAB_OIDC_' '${tmpl.path}'; then
          echo "ERROR: unreplaced placeholders in ${tmpl.path}" >&2
          exit 1
        fi
      '') templatesList}
    ''}

    echo "All secrets for ${ownerName} are ready"
  '';

  # Collision detection for explicit GIDs
  explicitGids = lib.filter (g: g != null) (lib.mapAttrsToList (_: owner: owner.gid) allOwners);
  dupGids = lib.filter (gid: lib.count (g: g == gid) explicitGids > 1) (lib.unique explicitGids);

  oidcProvisionedTarget = cfg.oidc.systemd.provisionedTarget;
in {
  config = lib.mkIf (allOwners != { }) {
    assertions = [
      {
        assertion = dupGids == [];
        message = "Service secrets have duplicate explicit gids: ${toString dupGids}";
      }
      {
        assertion = lib.intersectAttrs serviceOwners standaloneOwners == { };
        message = "Standalone secrets collide with service secrets: ${toString (lib.attrNames (lib.intersectAttrs serviceOwners standaloneOwners))}";
      }
    ];

    # Create groups for each owner
    users.groups = lib.mapAttrs' (_: owner:
      lib.nameValuePair owner.group (lib.optionalAttrs (owner.gid != null) {
        gid = owner.gid;
      })
    ) allOwners;

    # Create generator services and wire dependencies
    systemd.services = lib.mkMerge [
      # Generator services (one per owner with secrets)
      (lib.mapAttrs' (name: owner:
        lib.nameValuePair "homelab-secrets-${name}" {
          description = "Generate secrets for ${name}";
          wantedBy = [ "multi-user.target" ];
          before = map (s: "${s}.service") owner.systemd.dependentServices;

          # Auto-detected dependencies (ordering + restart propagation)
          after =
            (map (dep: "homelab-secrets-${dep}.service") (crossDeps name owner))
            ++ lib.optional (needsOidc owner && oidcProvisionedTarget != null) oidcProvisionedTarget;
          requires =
            (map (dep: "homelab-secrets-${dep}.service") (crossDeps name owner))
            ++ lib.optional (needsOidc owner && oidcProvisionedTarget != null) oidcProvisionedTarget;
          partOf =
            (map (dep: "homelab-secrets-${dep}.service") (crossDeps name owner))
            ++ lib.optional (needsOidc owner && oidcProvisionedTarget != null) oidcProvisionedTarget;

          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = mkGeneratorScript name owner;
          };
        }
      ) allOwners)

      # Wire dependencies into dependent services
      (lib.mkMerge (lib.mapAttrsToList (name: owner:
        lib.listToAttrs (map (depSvc: {
          name = depSvc;
          value = {
            requires = [ "homelab-secrets-${name}.service" ];
            after = [ "homelab-secrets-${name}.service" ];
            partOf = [ "homelab-secrets-${name}.service" ];
            serviceConfig.SupplementaryGroups = [ owner.group ];
          };
        }) owner.systemd.dependentServices)
      ) allOwners))
    ];

    # Warnings for non-rotatable secrets
    warnings = lib.flatten (lib.mapAttrsToList (ownerName: owner:
      lib.mapAttrsToList (fileName: file:
        lib.optional (!file.rotatable)
          "Secret '${ownerName}/${fileName}' is non-rotatable. Deleting it will regenerate, which may desync external systems."
      ) owner.files
    ) allOwners);
  };
}
