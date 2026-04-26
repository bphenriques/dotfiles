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
      password-file = cfg.passwordFile;
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
  servicesWithBackup = lib.filterAttrs (_: svc: svc.backup.package != null) homelabCfg.services;

  # Unit names (with .service suffix) for systemd dependency fields
  serviceHookUnits = lib.mapAttrsToList (name: _: "homelab-backup-${name}.service") servicesWithBackup;
  standaloneHookUnits = lib.mapAttrsToList (name: _: "homelab-backup-${name}.service") cfg.hooks;
  allHookUnits = serviceHookUnits ++ standaloneHookUnits;

  # Service IDs (without .service suffix) for NixOS systemd.services keys / ntfy injection
  allHookServiceIds =
    (lib.mapAttrsToList (name: _: "homelab-backup-${name}") servicesWithBackup)
    ++ (lib.mapAttrsToList (name: _: "homelab-backup-${name}") cfg.hooks);
in
{
  options.custom.homelab.backup = {
    enable = lib.mkEnableOption "homelab backup to Backblaze B2";

    package = lib.mkOption {
      type = lib.types.package;
      description = "The rustic-manage package to use.";
    };

    passwordFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to the rustic repository password file";
    };

    secretsFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to the rendered B2 credentials TOML file (symlinked to /etc/rustic/secrets.toml)";
    };

    src = lib.mkOption {
      type = lib.types.str;
      default = "${stateDir}/src";
      readOnly = true;
      description = "Working directory root assembled for backup.";
    };

    bindings = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = { "/nas/bphenriques/notes" = "/mnt/nas/bphenriques/notes"; };
      description = "Mapping of virtual backup path (key, must start with '/') to source path (value) for read-only bind mounts.";
    };

    hooks = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
        options = {
          package = lib.mkOption {
            type = lib.types.package;
            description = "Package providing backup script. Use writeShellApplication with runtimeInputs for dependencies. OUTPUT_DIR is provided as an environment variable pointing to a fresh, empty directory for the hook's output.";
          };
          after = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
          };
          outputDir = lib.mkOption {
            type = lib.types.str;
            default = "${extrasDir}/${name}";
            readOnly = true;
            description = "Directory where the hook writes its output. Available as OUTPUT_DIR in the hook environment.";
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

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
    assertions = let
      invalidKeys = lib.filter (k: !(lib.hasPrefix "/" k)) (lib.attrNames cfg.bindings);
    in [{
      assertion = invalidKeys == [];
      message = "Backup binding keys must be absolute paths (start with '/'). Invalid: ${toString invalidKeys}";
    }];

    custom.homelab.tasks.backup.systemdServices = lib.mkAfter allHookServiceIds;

    systemd.services = let
      mkHookService = name: { package, outputDir, after, ... }: lib.nameValuePair "homelab-backup-${name}" {
        description = "Pre-backup hook: ${name}";
        after = [ "network-online.target" ] ++ after;
        wants = [ "network-online.target" ] ++ after;
        environment.OUTPUT_DIR = outputDir;
        serviceConfig = {
          Type = "oneshot";
          ExecStartPre = [
            "${pkgs.coreutils}/bin/rm -rf -- ${outputDir}"
            "${pkgs.coreutils}/bin/install -d -m 0750 -o root -g root -- ${outputDir}"
          ];
          ExecStart = lib.getExe package;
          TimeoutStartSec = "30min";
          ReadWritePaths = [ extrasDir ];
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
        after = [ "network-online.target" "remote-fs.target" "ntfy-configure.service" ] ++ allHookUnits;
        wants = [ "network-online.target" "ntfy-configure.service" ] ++ allHookUnits;
        unitConfig.RequiresMountsFor = lib.attrValues cfg.bindings;
        environment = rusticManageEnv;
        serviceConfig = hardenedServiceConfig // {
          ExecStart = "${pkgs.util-linux}/bin/flock -w 21600 ${stateDir}/lock ${rusticManage} backup";
          ExecStartPost = "${pkgs.findutils}/bin/find ${extrasDir} -mindepth 1 -maxdepth 1 -exec ${pkgs.coreutils}/bin/rm -rf -- {} +";
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
          TimeoutStartSec = "3h";
          StateDirectory = "homelab-backup";
          ReadWritePaths = [ stateDir ];
        };
      };
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.src} 0750 root root -"
      "d ${extrasDir} 0750 root root -"
      "d /etc/rustic 0755 root root -"
      "L+ /etc/rustic/rustic.toml - - - - ${rusticProfile}"
      "L+ /etc/rustic/secrets.toml - - - - ${cfg.secretsFile}"
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
  })
  ];
}
