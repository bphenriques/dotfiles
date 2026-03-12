{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab.tasks.backup;
  homelabCfg = config.custom.homelab;

  stateDir = "/var/lib/homelab-backup";
  cacheDir = "${stateDir}/cache";
  configDir = "${stateDir}/config";

  tomlFormat = pkgs.formats.toml { };
  rusticProfile = tomlFormat.generate "homelab.toml" {
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

  # rustic -P expects paths without the .toml extension (it appends .toml itself)
  secretsProfilePath = lib.removeSuffix ".toml" config.sops.templates."homelab-backup-secrets.toml".path;
  ntfyCfg = config.custom.homelab.services.ntfy;

  rusticManage = "${cfg.package}/bin/rustic-manage-bin";
  rusticManageEnv = {
    RUSTIC_CACHE_DIR = cacheDir;
    RUSTIC_PROFILES = "${configDir}/homelab:${secretsProfilePath}";
    STATE_DIR = stateDir;
    NTFY_URL = "${ntfyCfg.url}/backups";
    NTFY_PASSWORD_FILE = ntfyCfg.secrets.files.admin-password.path;
  };

  # Services that have a backup script defined
  servicesWithBackup = lib.filterAttrs (_: svc: svc.backup.script != null) homelabCfg.services;
  serviceHookNames = lib.mapAttrsToList (name: _: "homelab-backup-${name}.service") servicesWithBackup;

  # Standalone hooks (e.g. github) not tied to a homelab service
  standaloneHookNames = lib.mapAttrsToList (name: _: "homelab-backup-${name}.service") cfg.hooks;

  allHookServiceNames = serviceHookNames ++ standaloneHookNames;
in
{
  options.custom.homelab.tasks.backup = {
    enable = lib.mkEnableOption "rustic backup to Backblaze B2";

    package = lib.mkOption {
      type = lib.types.package;
      description = "The rustic-manage package to use.";
    };

    src = lib.mkOption {
      type = lib.types.str;
      default = "${stateDir}/src";
      description = "Working directory root assembled for backup.";
    };

    extrasDir = lib.mkOption {
      type = lib.types.str;
      default = "${stateDir}/src/extras";
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
          description = lib.mkOption {
            type = lib.types.str;
            default = "Pre-backup hook: ${name}";
          };
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

    schedule = lib.mkOption {
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

  config = lib.mkIf cfg.enable {
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

    custom.homelab.services.ntfy.secrets.systemd.dependentServices = [
      "homelab-backup"
      "homelab-backup-verify"
    ];

    systemd.services = let
      mkHookService = name: { description, script, after, environment }: lib.nameValuePair "homelab-backup-${name}" {
        inherit description environment;
        after = [ "network-online.target" ] ++ after;
        wants = [ "network-online.target" ] ++ after;
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

      hardenedServiceConfig = {
        Type = "oneshot";
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
        after = [ "network-online.target" "remote-fs.target" ] ++ allHookServiceNames;
        wants = [ "network-online.target" ];
        requires = allHookServiceNames;
        unitConfig.RequiresMountsFor = lib.attrValues cfg.bindings;
        environment = rusticManageEnv;
        serviceConfig = hardenedServiceConfig // {
          ExecStart = "${rusticManage} backup";
          StateDirectory = "homelab-backup";
          ReadWritePaths = [ stateDir ];
          BindReadOnlyPaths = lib.mapAttrsToList (dst: src: "${src}:${cfg.src}${dst}") cfg.bindings;
        };
      };
      homelab-backup-verify = {
        description = "Homelab backup verification (Backblaze B2)";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        environment = rusticManageEnv;
        serviceConfig = hardenedServiceConfig // {
          ExecStart = "${rusticManage} verify";
          StateDirectory = "homelab-backup";
          ReadWritePaths = [ stateDir ];
        };
      };
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.src} 0750 root root -"
      "d ${cfg.extrasDir} 0750 root root -"
      "d ${cacheDir} 0750 root root -"
      "d ${configDir} 0750 root root -"
      "L+ ${configDir}/homelab.toml - - - - ${rusticProfile}"
    ]
    ++ lib.mapAttrsToList (dst: _: "d ${cfg.src}${dst} 0750 root root -") cfg.bindings;

    systemd.timers.homelab-backup = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.schedule;
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
