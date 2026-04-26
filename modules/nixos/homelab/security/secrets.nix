# Secret generation, templating, and per-owner group isolation. See hosts/compute/README.md.
{ lib, config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  secretsBaseDir = "/var/lib/homelab-secrets";

  mkPlaceholder = ownerName: secretName: "@HOMELAB_SECRET_${ownerName}_${secretName}@";

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

  # Auto-detect cross-owner dependencies by scanning template content for placeholder strings.
  # Template deps aren't structural (runtime substitution via replace-secret), so static analysis
  # can't resolve them. This eval-time string scan is the only way to derive the dependency graph.
  crossDeps = ownerName: ownerCfg:
    lib.filter (otherName: otherName != ownerName &&
      lib.any (tmpl: lib.hasInfix "@HOMELAB_SECRET_${otherName}_" tmpl.content)
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

    # Generate raw secret files (only if missing), always enforce ownership
    ${lib.concatMapStringsSep "\n" (file: ''
      if [ ! -f "${file.path}" ]; then
        echo "Generating secret: ${file.name}"
        ${pkgs.openssl}/bin/openssl rand -hex ${toString file.bytes} > "${file.path}"
      fi
      chown root:"$group" "${file.path}"
      chmod 640 "${file.path}"
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
        if ${pkgs.gnugrep}/bin/grep -qE '@HOMELAB_(SECRET|OIDC)_' '${tmpl.path}'; then
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

  # Eval-time validation: extract placeholder tokens from template content and verify they all resolve
  extractPlaceholders = content: let
    parts = builtins.split "(@HOMELAB_[A-Z]+_[A-Za-z0-9_-]+_[A-Za-z0-9_.-]+@)" content;
  in
    lib.unique (lib.concatMap (part:
      if lib.isList part then part else []
    ) parts);
in {
  options.custom.homelab.secrets = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submoduleWith {
      modules = [ ../schemas/secrets.nix ];
    });
    default = { };
    description = "Secrets for non-service owners (timers, tasks, etc.)";
  };

  config = lib.mkMerge [
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
      ] ++ lib.flatten (lib.mapAttrsToList (ownerName: ownerCfg:
        lib.mapAttrsToList (tmplName: tmpl: let
          found = extractPlaceholders tmpl.content;
          unknown = lib.filter (p: ! lib.hasAttr p allVars) found;
        in {
          assertion = unknown == [];
          message = "Unknown placeholders in ${ownerName}/templates/${tmplName}: ${lib.concatStringsSep ", " unknown}";
        }) ownerCfg.templates
      ) allOwners);

      users.groups = lib.mapAttrs' (_: owner:
        lib.nameValuePair owner.group (lib.optionalAttrs (owner.gid != null) {
          inherit (owner) gid;
        })
      ) allOwners;

      # Pre-create secrets directories so ProtectSystem=strict + ReadWritePaths works on first boot
      systemd.tmpfiles.rules = lib.mapAttrsToList (_: owner:
        "d ${owner.secretsDir} 0750 root ${owner.group} -"
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
              UMask = "0077";
              ProtectSystem = "strict";
              ReadWritePaths = [ owner.secretsDir ]
                ++ lib.unique (map (tmpl: builtins.dirOf tmpl.path) (lib.attrValues owner.templates));
              ProtectHome = true;
              PrivateTmp = true;
              NoNewPrivileges = true;
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
