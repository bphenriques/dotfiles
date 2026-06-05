# Runtime secret generation + template rendering, sops-shaped.
# One systemd unit generates random files and renders templates from a flat namespace.
{ lib, config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  secretsDir = "/var/lib/homelab-secrets";   # persistent
  templatesDir = "/run/homelab-secrets/templates"; # tmpfs; re-rendered each boot

  users = config.users.users;

  resolveGroup = item:
    if item.group != null then item.group
    else if users ? ${item.owner} then users.${item.owner}.group
    else item.owner;

  mkPlaceholder = key: "<HOMELAB:${builtins.hashString "sha256" key}:PLACEHOLDER>";

  secretPlaceholderMap = lib.mapAttrs (name: _: mkPlaceholder "secret:${name}") cfg.runtimeSecrets;

  oidcClients = cfg.oidc.clients or { };
  oidcPlaceholderMap = lib.mapAttrs (name: _: {
    id = mkPlaceholder "oidc:${name}:id";
    secret = mkPlaceholder "oidc:${name}:secret";
  }) oidcClients;

  # Substitution table: placeholder string -> file path containing the value.
  secretSubstitutions = lib.mapAttrs' (name: s:
    lib.nameValuePair (mkPlaceholder "secret:${name}") s.path
  ) cfg.runtimeSecrets;

  oidcSubstitutions = lib.concatMapAttrs (name: client: {
    "${mkPlaceholder "oidc:${name}:id"}" = client.id.file;
    "${mkPlaceholder "oidc:${name}:secret"}" = client.secret.file;
  }) oidcClients;

  allSubstitutions = secretSubstitutions // oidcSubstitutions;

  generateBranch = name: s: ''
    echo "Generating ${name}"
    tmp=$(mktemp -p "$(dirname "$path")" .tmp-XXXXXX)
    openssl rand -hex ${toString s.bytes} > "$tmp"
    mv -f "$tmp" "$path"
  '';

  fatalBranch = name: ''
    echo "FATAL: ${name} missing at $path and regenerateIfMissing=false." >&2
    echo "Restore from backup or set regenerateIfMissing=true to recreate." >&2
    exit 1
  '';

  mkSecretScript = name: s: ''
    path=${lib.escapeShellArg s.path}
    ${lib.optionalString (s.migrateFrom != null) ''
    if [ ! -e "$path" ] && [ -e ${lib.escapeShellArg s.migrateFrom} ]; then
      echo "Migrating ${name} from ${s.migrateFrom}"
      cp -p ${lib.escapeShellArg s.migrateFrom} "$path"
    fi
    ''}
    if [ ! -e "$path" ]; then
      ${if s.regenerateIfMissing then generateBranch name s else fatalBranch name}
    fi
    chown ${lib.escapeShellArg s.owner}:${lib.escapeShellArg (resolveGroup s)} "$path"
    chmod ${lib.escapeShellArg s.mode} "$path"
  '';

  # Filter placeholders down to those actually used in the template to avoid
  # opening every secret file on every render (matters when many OIDC clients exist).
  mkTemplateScript = name: t: let
    relevant = lib.filterAttrs (placeholder: _: lib.hasInfix placeholder t.content) allSubstitutions;
    srcFile = pkgs.writeText "homelab-template-${name}" t.content;
  in ''
    echo "Rendering ${name}"
    path=${lib.escapeShellArg t.path}
    install -D -m ${lib.escapeShellArg t.mode} \
      -o ${lib.escapeShellArg t.owner} \
      -g ${lib.escapeShellArg (resolveGroup t)} \
      ${srcFile} "$path"
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (placeholder: filePath: ''
      replace-secret ${lib.escapeShellArg placeholder} ${lib.escapeShellArg filePath} "$path"
    '') relevant)}
    if grep -qE '<HOMELAB:[a-f0-9]+:PLACEHOLDER>' "$path"; then
      echo "FATAL: unresolved placeholders in $path" >&2
      exit 1
    fi
  '';

  allRestartUnits = lib.unique (
    (lib.concatLists (lib.mapAttrsToList (_: s: s.restartUnits) cfg.runtimeSecrets))
    ++ (lib.concatLists (lib.mapAttrsToList (_: t: t.restartUnits) cfg.runtimeTemplates))
  );

  templateParentDirs = lib.unique (map (t: builtins.dirOf t.path) (lib.attrValues cfg.runtimeTemplates));

  secretSubmodule = { name, ... }: {
    options = {
      bytes = lib.mkOption {
        type = lib.types.int;
        default = 32;
        description = "Random bytes (hex-encoded; file is 2x chars).";
      };
      regenerateIfMissing = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Generate a new random value when the file is missing.
          Set false for bootstrap secrets synced with external state — missing file then fails the unit instead of silently re-seeding.
        '';
      };
      migrateFrom = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Optional source path to copy from on first boot if the new path doesn't exist.
          Used during migration from the legacy per-owner layout (cp -p, leaves the source in place for rollback).
        '';
      };
      owner = lib.mkOption {
        type = lib.types.str;
        default = "root";
        description = "Unix owner of the file.";
      };
      group = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Unix group; defaults to owner's primary group.";
      };
      mode = lib.mkOption {
        type = lib.types.str;
        default = "0400";
      };
      path = lib.mkOption {
        type = lib.types.str;
        default = "${secretsDir}/${name}";
        readOnly = true;
      };
      restartUnits = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          Units that consume this secret. Wired with requires + after on the generator.
          Note: secret content doesn't change between deploys (random + persistent),
          so this provides ordering only — manual rotation requires manually restarting consumers.
        '';
      };
    };
  };

  templateSubmodule = { name, ... }: {
    options = {
      content = lib.mkOption {
        type = lib.types.lines;
        description = ''
          Template body. Reference secrets via config.custom.homelab.runtimePlaceholder.<secretName>
          and OIDC credentials via config.custom.homelab.oidcPlaceholder.<clientName>.{id,secret}.
        '';
      };
      path = lib.mkOption {
        type = lib.types.str;
        default = "${templatesDir}/${name}";
        description = "Rendered output path (tmpfs; regenerated each boot).";
      };
      owner = lib.mkOption {
        type = lib.types.str;
        default = "root";
      };
      group = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
      mode = lib.mkOption {
        type = lib.types.str;
        default = "0400";
      };
      restartUnits = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Units restarted when the template body changes between deploys.";
      };
    };
  };
in
{
  options.custom.homelab = {
    runtimeSecretsDir = lib.mkOption {
      type = lib.types.str;
      default = secretsDir;
      readOnly = true;
      description = "Persistent directory containing runtime-generated secret files. Include in backups.";
    };

    runtimeSecrets = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule secretSubmodule);
      default = { };
      description = "Runtime-generated secret files (one-shot openssl rand).";
    };

    runtimeTemplates = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule templateSubmodule);
      default = { };
      description = "Templates rendered from runtime secrets and OIDC credentials.";
    };

    runtimePlaceholder = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = secretPlaceholderMap;
      readOnly = true;
      description = "Opaque placeholder string per declared runtime secret.";
    };

    oidcPlaceholder = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          id = lib.mkOption { type = lib.types.str; };
          secret = lib.mkOption { type = lib.types.str; };
        };
      });
      default = oidcPlaceholderMap;
      readOnly = true;
      description = "Opaque placeholder pair per OIDC client.";
    };
  };

  config = lib.mkIf (cfg.runtimeSecrets != { } || cfg.runtimeTemplates != { }) {
    systemd.tmpfiles.rules = [
      "d ${secretsDir} 0755 root root -"
    ] ++ lib.optional (cfg.runtimeTemplates != { })
      "d ${templatesDir} 0755 root root -";

    systemd.services = lib.mkMerge (
      [{
        homelab-runtime-secrets = {
          description = "Generate runtime secrets and render templates";
          wantedBy = [ "multi-user.target" ];
          before = allRestartUnits;
          path = with pkgs; [ coreutils openssl replace-secret gnugrep ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            UMask = "0077";
            ProtectSystem = "strict";
            ReadWritePaths = [ secretsDir ] ++ templateParentDirs;
            ProtectHome = true;
            PrivateTmp = true;
            NoNewPrivileges = true;
          };
          script = ''
            set -euo pipefail
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkSecretScript cfg.runtimeSecrets)}
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkTemplateScript cfg.runtimeTemplates)}
          '';
        };
      }]
      ++ (lib.mapAttrsToList (_: s:
        lib.listToAttrs (map (unit: {
          name = lib.removeSuffix ".service" unit;
          value = {
            after = [ "homelab-runtime-secrets.service" ];
            requires = [ "homelab-runtime-secrets.service" ];
          };
        }) s.restartUnits)
      ) cfg.runtimeSecrets)
      ++ (lib.mapAttrsToList (_: t:
        lib.listToAttrs (map (unit: {
          name = lib.removeSuffix ".service" unit;
          value = {
            after = [ "homelab-runtime-secrets.service" ];
            requires = [ "homelab-runtime-secrets.service" ];
            restartTriggers = [ t.content ];
          };
        }) t.restartUnits)
      ) cfg.runtimeTemplates)
    );
  };
}
