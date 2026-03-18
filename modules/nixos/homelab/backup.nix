{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab.backup;
  homelabCfg = config.custom.homelab;

  stateDir = "/var/lib/homelab-backup";
  extrasDir = "${stateDir}/src/extras";

  ntfyCfg = config.custom.homelab.services.ntfy;
  backupTaskCfg = config.custom.homelab.tasks.backup;

  tomlFormat = pkgs.formats.toml { };
  rusticProfile = tomlFormat.generate "homelab.toml" {
    global.use-profiles = ["secrets"];
    repository = {
      repository = "opendal:b2";
      password-file = config.sops.secrets."backup/rustic/password".path;
    };

    backup = {
      exclude-if-present = [ ".nobackup" ];
      git-ignore = true;
      no-require-git = true;
      globs = [
        "!@eaDir"
        "!.stfolder"
        "!.Trash*"
      ];
      snapshots = [{ sources = [ cfg.src ]; }];
    };

    forget = {
      prune = true;
      keep-within-daily = "7 days";
      keep-within-weekly = "1 month";
      keep-within-monthly = "1 year";
      keep-within-yearly = "2 years";
    };
  };

  rusticManage = "${cfg.package}/bin/rustic-manage-bin";
  rusticManageEnv = {
    STATE_DIR = stateDir;
    NTFY_URL = "${ntfyCfg.url}/${backupTaskCfg.integrations.ntfy.topic}";
    NTFY_TOKEN_FILE = backupTaskCfg.integrations.ntfy.tokenFile;
  };

  # Services that have a backup script defined
  servicesWithBackup = lib.filterAttrs (_: svc: svc.backup.script != null) homelabCfg.services;

  serviceHookNames = lib.mapAttrsToList (name: _: "homelab-backup-${name}.service") servicesWithBackup;
  standaloneHookNames = lib.mapAttrsToList (name: _: "homelab-backup-${name}.service") cfg.hooks;
  allHookServiceNames = serviceHookNames ++ standaloneHookNames;
in
{
  options.custom.homelab.backup = {
    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "The rustic-manage package to use.";
    };

    src = lib.mkOption {
      type = lib.types.str;
      default = "${stateDir}/src";
      readOnly = true;
      description = "Working directory root assembled for backup.";
    };

    extrasDir = lib.mkOption {
      type = lib.types.str;
      default = extrasDir;
      readOnly = true;
      description = "Directory for extra files to include in the backup. Populate via service backup hooks.";
    };

    bindings = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = { "/nas/bphenriques/notes" = "/mnt/nas/bphenriques/notes"; };
      description = "Mapping of destination folder (relative to src) to source path for read-only bind mounts.";
    };

    hooks = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
        options = {
          script = lib.mkOption { type = lib.types.path; };
          after = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
          };
          environment = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = { };
          };
        };
      }));
      default = { };
      description = "Standalone pre-backup hooks not tied to a homelab service (e.g. GitHub).";
    };

    backupSchedule = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* 03:00:00";
      description = "systemd OnCalendar schedule for the backup timer.";
    };

    verifySchedule = lib.mkOption {
      type = lib.types.str;
      default = "Sun *-*-* 05:00:00";
      description = "systemd OnCalendar schedule for the weekly verification timer.";
    };
  };

  config = lib.mkIf (cfg.package != null) {
    sops = {
      secrets."backup/b2/bucket" = { };
      secrets."backup/b2/bucket_id" = { };
      secrets."backup/b2/application_key_id" = { };
      secrets."backup/b2/application_key" = { };
      secrets."backup/rustic/password" = { };
      templates."homelab-backup-secrets.toml" = {
        owner = "root";
        group = "root";
        mode = "0400";
        content = ''
          [repository.options]
          bucket = "${config.sops.placeholder."backup/b2/bucket"}"
          bucket_id = "${config.sops.placeholder."backup/b2/bucket_id"}"
          application_key_id = "${config.sops.placeholder."backup/b2/application_key_id"}"
          application_key = "${config.sops.placeholder."backup/b2/application_key"}"
        '';
      };
    };

    systemd.services = let
      mkHookService = name: { script, after, environment }: lib.nameValuePair "homelab-backup-${name}" {
        inherit environment;
        description = "Pre-backup hook: ${name}";
        after = [ "network-online.target" ] ++ after;
        wants = [ "network-online.target" ];
        path = [ pkgs.coreutils pkgs.curl ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.bash}/bin/bash ${script}";
          ReadWritePaths = [ cfg.extrasDir ];
          ProtectSystem = "strict";
          ProtectHome = true;
          PrivateTmp = true;
          NoNewPrivileges = true;
        };
      };

      serviceHooks = lib.mapAttrs' (name: svc: mkHookService name svc.backup) servicesWithBackup;
      standaloneHooks = lib.mapAttrs' (name: hook: mkHookService name hook) cfg.hooks;

      notifyTimeoutScript = pkgs.writeShellScript "backup-notify-timeout" ''
        if [ "''${SERVICE_RESULT:-}" = "timeout" ]; then
          NTFY_TOKEN="$(tr -d '\n' < "$NTFY_TOKEN_FILE")"
          ${pkgs.curl}/bin/curl -fsS --max-time 10 \
            -H "Authorization: Bearer $NTFY_TOKEN" \
            -H "Title: Backup Failed" \
            -H "Tags: x" \
            -H "Priority: high" \
            -d "$1 timed out" \
            "$NTFY_URL" || true
        fi
      '';

      hardenedServiceConfig = {
        Type = "oneshot";
        User = "root";
        Group = "root";
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
      };
    in serviceHooks // standaloneHooks // {
      homelab-backup = {
        description = "Homelab backup (Backblaze B2)";
        after = [ "network-online.target" "remote-fs.target" "ntfy-configure.service" ] ++ allHookServiceNames;
        wants = [ "network-online.target" "ntfy-configure.service" ];
        requires = allHookServiceNames;
        unitConfig.RequiresMountsFor = lib.attrValues cfg.bindings;
        environment = rusticManageEnv;
        serviceConfig = hardenedServiceConfig // {
          ExecStart = "${pkgs.util-linux}/bin/flock -w 21600 ${stateDir}/lock ${rusticManage} backup";
          ExecStopPost = "${notifyTimeoutScript} Backup";
          TimeoutStartSec = "6h";
          StateDirectory = "homelab-backup";
          ReadWritePaths = [ stateDir ];
          BindReadOnlyPaths = lib.mapAttrsToList (dst: src: "${src}:${cfg.src}${dst}") cfg.bindings;
        };
      };
      homelab-backup-verify = {
        description = "Homelab backup verification (Backblaze B2)";
        after = [ "network-online.target" "ntfy-configure.service" ];
        wants = [ "network-online.target" "ntfy-configure.service" ];
        environment = rusticManageEnv;
        serviceConfig = hardenedServiceConfig // {
          ExecStart = "${pkgs.util-linux}/bin/flock -w 10800 ${stateDir}/lock ${rusticManage} verify";
          ExecStopPost = "${notifyTimeoutScript} Verification";
          TimeoutStartSec = "3h";
          StateDirectory = "homelab-backup";
          ReadWritePaths = [ stateDir ];
        };
      };
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.src} 0750 root root -"
      "d ${cfg.extrasDir} 0750 root root -"
      "d /etc/rustic 0755 root root -"
      "L+ /etc/rustic/rustic.toml - - - - ${rusticProfile}"
      "L+ /etc/rustic/secrets.toml - - - - ${config.sops.templates."homelab-backup-secrets.toml".path}"
    ]
    ++ lib.mapAttrsToList (dst: _: "d ${cfg.src}${dst} 0750 root root -") cfg.bindings;

    systemd.timers.homelab-backup = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.backupSchedule;
        RandomizedDelaySec = "1h";
        Persistent = true;
      };
    };

    systemd.timers.homelab-backup-verify = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.verifySchedule;
        RandomizedDelaySec = "1h";
        Persistent = true;
      };
    };
  };
}
