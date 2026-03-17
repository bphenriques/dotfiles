# Implements secret generation and templating for services, tasks, and standalone owners.
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
  secretsBaseDir = "/var/lib/homelab-secrets";

  mkPlaceholder = ownerName: secretName: "__HOMELAB_SECRET_${ownerName}_${secretName}__";

  mkSecretsSchema = ownerName: { config, ... }: {
    options = {
      files = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
          options = {
            bytes = lib.mkOption {
              type = lib.types.int;
              default = 32;
              description = "Number of random bytes to generate (output file will be 2x chars due to hex encoding)";
            };

            rotatable = lib.mkOption {
              type = lib.types.bool;
              description = ''
                Whether this secret can be safely rotated (delete + restart).
                Set to false for bootstrap secrets that sync with external systems.
              '';
            };

            path = lib.mkOption {
              type = lib.types.str;
              default = "${secretsBaseDir}/${ownerName}/${name}";
              readOnly = true;
              description = "Path to the generated secret file";
            };
          };
        }));
        default = { };
        description = "Secret files to generate for this owner";
      };

      placeholder = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = lib.mapAttrs (name: _: mkPlaceholder ownerName name) config.files;
        readOnly = true;
        description = "Placeholder strings for each secret file. Use in template content.";
      };

      templates = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
          options = {
            content = lib.mkOption {
              type = lib.types.lines;
              description = ''
                Template content with placeholders. Use secrets.placeholder.<name> for substitution.
                Placeholders are replaced at runtime using replace-secret.
              '';
            };

            path = lib.mkOption {
              type = lib.types.str;
              default = "${secretsBaseDir}/${ownerName}/${name}";
              description = "Path where the rendered template will be written";
            };
          };
        }));
        default = { };
        description = "Template files derived from this owner's secrets";
      };

      group = lib.mkOption {
        type = lib.types.str;
        default = "homelab-secrets-${ownerName}";
        readOnly = true;
        description = "Group name for accessing this owner's secrets";
      };

      gid = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = "Fixed GID for the secrets group (null = auto-assign)";
      };

      secretsDir = lib.mkOption {
        type = lib.types.str;
        default = "${secretsBaseDir}/${ownerName}";
        readOnly = true;
        description = "Directory containing this owner's secrets";
      };

      systemd.dependentServices = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          Systemd services that depend on this owner's secrets.
          Automatically wires requires/after/partOf/SupplementaryGroups.
        '';
      };
    };
  };

  # Collect secret owners from services, tasks, and standalone secrets
  serviceOwners = lib.mapAttrs (_: svc: svc.secrets) (lib.filterAttrs (_: svc:
    svc.secrets.files != { } || svc.secrets.templates != { }
  ) cfg.services);

  taskOwners = lib.mapAttrs (_: task: task.secrets) (lib.filterAttrs (_: task:
    task.secrets.files != { } || task.secrets.templates != { }
  ) cfg.tasks);

  standaloneOwners = lib.filterAttrs (_: owner:
    owner.files != { } || owner.templates != { }
  ) cfg.secrets;

  allOwners = serviceOwners // taskOwners // standaloneOwners;

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

  # Auto-detect which OIDC clients are referenced in template content (by placeholder patterns)
  oidcClientNames = lib.attrNames (lib.filterAttrs (_: svc: svc.oidc.enable) cfg.services);
  oidcClientDeps = ownerCfg:
    lib.filter (clientName:
      lib.any (tmpl: lib.hasInfix "@HOMELAB_OIDC_${clientName}_" tmpl.content)
        (lib.attrValues ownerCfg.templates)
    ) oidcClientNames;

  # Helper to generate the shell script for an owner's secrets
  mkGeneratorScript = ownerName: ownerCfg: let
    filesList = lib.mapAttrsToList (name: file: {
      inherit name;
      inherit (file) bytes path;
    }) ownerCfg.files;

    templatesList = lib.mapAttrsToList (name: tmpl: {
      inherit name;
      inherit (tmpl) path content;
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
      ${lib.concatMapStringsSep "\n" (tmpl: let
        relevantVars = lib.filterAttrs (placeholder: _: lib.hasInfix placeholder tmpl.content) allVars;
      in ''
        echo "Rendering template: ${tmpl.name}"
        mkdir -p "$(dirname '${tmpl.path}')"
        install -m 640 -o root -g "$group" "${tmpl.srcPath}" "${tmpl.path}"
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (placeholder: filePath: ''
          ${pkgs.replace-secret}/bin/replace-secret \
            '${placeholder}' \
            '${filePath}' \
            '${tmpl.path}'
        '') relevantVars)}
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

  oidcBaseUnit = cfg.oidc.systemd.baseProvisionUnit;
  oidcClientUnitPrefix = cfg.oidc.systemd.clientProvisionUnitPrefix;
  mkOidcClientUnit = name: "${oidcClientUnitPrefix}${name}.service";
in {
  options.custom.homelab.secrets = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
      imports = [ (mkSecretsSchema name) ];
    }));
    default = { };
    description = "Secrets for non-service owners (timers, tasks, etc.)";
  };

  config = lib.mkMerge [
    {
      custom.homelab._serviceOptionExtensions = [
        ({ name, ... }: {
          options.secrets = lib.mkOption {
            type = lib.types.submodule (mkSecretsSchema name);
            default = { };
            description = "Generated secrets for this service";
          };
        })
      ];

      custom.homelab._taskOptionExtensions = [
        ({ name, ... }: {
          options.secrets = lib.mkOption {
            type = lib.types.submodule (mkSecretsSchema name);
            default = { };
            description = "Generated secrets for this task";
          };
        })
      ];
    }

    (lib.mkIf (allOwners != { }) {
      assertions = let
        ownerSources = lib.concatMap (owners: lib.attrNames owners) [ serviceOwners taskOwners standaloneOwners ];
        dupNames = lib.filter (n: lib.count (x: x == n) ownerSources > 1) (lib.unique ownerSources);
      in [
        {
          assertion = dupGids == [];
          message = "Service secrets have duplicate explicit gids: ${toString dupGids}";
        }
        {
          assertion = dupNames == [];
          message = "Secret owner name collision across services/tasks/standalone: ${toString dupNames}";
        }
      ];

      users.groups = lib.mapAttrs' (_: owner:
        lib.nameValuePair owner.group (lib.optionalAttrs (owner.gid != null) {
          gid = owner.gid;
        })
      ) allOwners;

      systemd.services = lib.mkMerge [
        (lib.mapAttrs' (name: owner:
          lib.nameValuePair "homelab-secrets-${name}" {
            description = "Generate secrets for ${name}";
            wantedBy = [ "multi-user.target" ];
            before = map (s: "${s}.service") owner.systemd.dependentServices;

            after =
              (map (dep: "homelab-secrets-${dep}.service") (crossDeps name owner))
              ++ lib.optionals (oidcBaseUnit != null) (map mkOidcClientUnit (oidcClientDeps owner));
            requires =
              (map (dep: "homelab-secrets-${dep}.service") (crossDeps name owner))
              ++ lib.optionals (oidcBaseUnit != null) (map mkOidcClientUnit (oidcClientDeps owner));
            partOf =
              (map (dep: "homelab-secrets-${dep}.service") (crossDeps name owner))
              ++ lib.optionals (oidcBaseUnit != null) (map mkOidcClientUnit (oidcClientDeps owner));

            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              ExecStart = mkGeneratorScript name owner;
            };
          }
        ) allOwners)

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

      warnings = lib.flatten (lib.mapAttrsToList (ownerName: owner:
        lib.mapAttrsToList (fileName: file:
          lib.optional (!file.rotatable)
            "Secret '${ownerName}/${fileName}' is non-rotatable. Deleting it will regenerate, which may desync external systems."
        ) owner.files
      ) allOwners);
    })
  ];
}
