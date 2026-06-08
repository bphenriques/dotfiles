# Rustic backup: one isolated, independent pipeline per target (own content, repo, retention, schedule).
{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab.backup;
  homelabCfg = config.custom.homelab;

  stateDir = "/var/lib/homelab-backup";
  targetRoot = name: "${stateDir}/${name}";
  targetSrc = name: "${targetRoot name}/src";
  targetExtras = name: "${targetSrc name}/extras";

  notifyCfg = config.custom.homelab.notify;
  backupTaskCfg = config.custom.homelab.tasks.backup;

  rusticManage = "${cfg.package}/bin/rustic-manage-bin";
  rusticManageEnv = {
    STATE_DIR = stateDir;
    SEND_NOTIFICATION = "${notifyCfg.package}/bin/send-notification";
    NOTIFY_URL = notifyCfg.url;
    NOTIFY_TOPIC = backupTaskCfg.integrations.notify.topic;
    NOTIFY_TOKEN_FILE = backupTaskCfg.integrations.notify.tokenFile;
  };

  tomlFormat = pkgs.formats.toml { };

  servicesWithBackup = lib.filterAttrs (_: svc: svc.backup.package != null) homelabCfg.services;

  resolveHooks = t:
    lib.genAttrs t.services (svcName: { inherit (homelabCfg.services.${svcName}.backup) package after; })
    // lib.mapAttrs (_: h: { inherit (h) package after; }) t.hooks;

  mkRusticProfile = name: t: tomlFormat.generate "rustic-${name}.toml" ({
    repository = {
      inherit (t) repository;
      password-file = t.passwordFile;
    };
    backup = {
      exclude-if-present = [ ".nobackup" ];
      git-ignore = true;
      no-require-git = true;
      globs = [ "!@eaDir" "!.stfolder" "!.Trash*" ];
      snapshots = [{ sources = [ (targetSrc name) ]; }];
    };
    forget = {
      prune = true;
      keep-within-daily = t.retention.daily;
      keep-within-weekly = t.retention.weekly;
      keep-within-monthly = t.retention.monthly;
      keep-within-yearly = t.retention.yearly;
    };
    # Backend credentials live in a sops-rendered secrets profile; omit for local (path) repos.
  } // lib.optionalAttrs (t.backendCredentialsFile != null) {
    global.use-profiles = [ "${name}-secrets" ];
  });

  mkAssembleScript = name: hooks: pkgs.writeShellApplication {
    name = "homelab-backup-assemble-${name}";
    runtimeInputs = [ pkgs.coreutils ];
    text = lib.concatStrings (lib.mapAttrsToList (hookName: hook: ''
      echo "Hook: ${hookName}"
      rm -rf -- "${targetExtras name}/${hookName}"
      install -d -m 0750 -- "${targetExtras name}/${hookName}"
      OUTPUT_DIR="${targetExtras name}/${hookName}" ${lib.getExe hook.package}
    '') hooks);
  };

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
    ReadWritePaths = [ stateDir ];
  };

  mkBackupService = name: t:
    let
      hooks = resolveHooks t;
      hookDeps = lib.unique (lib.concatMap (h: h.after) (lib.attrValues hooks));
    in
    lib.nameValuePair "homelab-backup-${name}" {
      description = "Homelab backup: ${name}";
      after = [ "network-online.target" "remote-fs.target" "ntfy-configure.service" ] ++ hookDeps;
      wants = [ "network-online.target" "ntfy-configure.service" ];
      requires = hookDeps;
      unitConfig.RequiresMountsFor = lib.attrValues t.bindings
        ++ lib.optional (lib.hasPrefix "/" t.repository) t.repository;
      environment = rusticManageEnv;
      serviceConfig = hardenedServiceConfig // {
        ExecStartPre = lib.getExe (mkAssembleScript name hooks);
        ExecStart = "${rusticManage} backup ${name}";
        # Runs on failure too; list form merges with the failure-notify ExecStopPost.
        ExecStopPost = [ "${pkgs.findutils}/bin/find ${targetExtras name} -mindepth 1 -maxdepth 1 -exec ${pkgs.coreutils}/bin/rm -rf -- {} +" ];
        TimeoutStartSec = "6h";
        BindReadOnlyPaths = lib.mapAttrsToList (dst: src: "${src}:${targetSrc name}${dst}") t.bindings;
      };
    };

  mkVerifyService = name: t: lib.nameValuePair "homelab-backup-${name}-verify" {
    description = "Homelab backup verification: ${name}";
    after = [ "network-online.target" "ntfy-configure.service" ];
    wants = [ "network-online.target" "ntfy-configure.service" ];
    unitConfig.RequiresMountsFor = lib.optional (lib.hasPrefix "/" t.repository) t.repository;
    environment = rusticManageEnv;
    serviceConfig = hardenedServiceConfig // {
      ExecStart = "${rusticManage} verify ${name}";
      TimeoutStartSec = "3h";
    };
  };

  mkTimer = name: suffix: schedule: lib.nameValuePair "homelab-backup-${name}${suffix}" {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = schedule;
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };

  hookModule = lib.types.submodule {
    options = {
      package = lib.mkOption {
        type = lib.types.package;
        description = "Package providing a backup script. OUTPUT_DIR points to a fresh, empty directory for the hook's output.";
      };
      after = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
    };
  };
in
{
  options.custom.homelab.backup = {
    package = lib.mkOption {
      type = lib.types.package;
      description = "The rustic-manage package to use (shared by all targets).";
    };

    targets = lib.mkOption {
      default = { };
      description = "Backup destinations. Each is an independent rustic pipeline with its own content, repository, retention, and schedule.";
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          repository = lib.mkOption {
            type = lib.types.str;
            description = "rustic repository string (e.g. 'opendal:b2').";
          };
          backendCredentialsFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Storage backend access credentials as a rustic secrets TOML ([repository.options]); symlinked to /etc/rustic/<name>-secrets.toml. Null for local-path repositories (no backend auth).";
          };
          passwordFile = lib.mkOption {
            type = lib.types.str;
            description = "rustic repository encryption password file.";
          };
          retention = lib.mkOption {
            description = "Snapshot retention policy (rustic forget keep-within-*).";
            type = lib.types.submodule {
              options = {
                daily = lib.mkOption { type = lib.types.str; };
                weekly = lib.mkOption { type = lib.types.str; };
                monthly = lib.mkOption { type = lib.types.str; };
                yearly = lib.mkOption { type = lib.types.str; };
              };
            };
          };

          bindings = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = { };
            example = { "/nas/notes" = "/mnt/nas/notes"; };
            description = "Virtual backup path (key, must start with '/') -> source path (value), mounted read-only into this target's tree.";
          };
          services = lib.mkOption {
            type = lib.types.listOf (lib.types.enum (lib.attrNames servicesWithBackup));
            default = [ ];
            description = "Registry services whose backup hook to run for this target (must declare services.<name>.backup.package).";
          };
          hooks = lib.mkOption {
            type = lib.types.attrsOf hookModule;
            default = { };
            description = "Standalone pre-backup hooks not tied to a registry service (e.g. GitHub), run for this target.";
          };

          backupSchedule = lib.mkOption {
            type = lib.types.str;
            default = "*-*-* 03:00:00";
            description = "systemd OnCalendar schedule for the backup timer.";
          };
          verifySchedule = lib.mkOption {
            type = lib.types.str;
            default = "Sun *-*-* 05:00:00";
            description = "systemd OnCalendar schedule for the verification timer.";
          };
        };
      });
    };
  };

  config = lib.mkIf (cfg.targets != { }) {
    assertions = let
      badBindingKeys = lib.filter
        (k: !(lib.hasPrefix "/" k) || k == "/extras" || lib.hasPrefix "/extras/" k)
        (lib.concatMap (t: lib.attrNames t.bindings) (lib.attrValues cfg.targets));

      nameCollisions = lib.concatLists (lib.mapAttrsToList (name: t:
        map (n: "${name}.${n}") (lib.intersectLists t.services (lib.attrNames t.hooks))
      ) cfg.targets);

      referencedServices = lib.unique (lib.concatMap (t: t.services) (lib.attrValues cfg.targets));
      orphanServices = lib.subtractLists referencedServices (lib.attrNames servicesWithBackup);
    in [
      {
        assertion = badBindingKeys == [ ];
        message = "Backup binding keys must be absolute paths outside the reserved /extras namespace: ${toString badBindingKeys}";
      }
      {
        assertion = nameCollisions == [ ];
        message = "Backup service and standalone hooks share a name (same extras dir): ${toString nameCollisions}";
      }
      {
        assertion = orphanServices == [ ];
        message = "Services declare a backup.package but no target includes them (never backed up): ${toString orphanServices}";
      }
    ];

    # A content-less target is allowed (e.g. a repo connectivity test) but usually a mistake.
    warnings = let
      empty = lib.attrNames (lib.filterAttrs (_: t: t.bindings == { } && t.services == [ ] && t.hooks == { }) cfg.targets);
    in lib.optional (empty != [ ]) "Backup target(s) snapshot an empty tree (no bindings/services/hooks): ${toString empty}";

    custom.homelab.notify.topics."homelab-backup".public = lib.mkDefault false;
    custom.homelab.tasks.backup.integrations.notify.topic = lib.mkDefault "homelab-backup";

    custom.homelab.tasks.backup.systemdServices =
      lib.concatMap (name: [ "homelab-backup-${name}" "homelab-backup-${name}-verify" ]) (lib.attrNames cfg.targets);

    systemd.services = lib.listToAttrs (
      lib.mapAttrsToList mkBackupService cfg.targets
      ++ lib.mapAttrsToList mkVerifyService cfg.targets
    );

    systemd.timers = lib.listToAttrs (
      lib.mapAttrsToList (name: t: mkTimer name "" t.backupSchedule) cfg.targets
      ++ lib.mapAttrsToList (name: t: mkTimer name "-verify" t.verifySchedule) cfg.targets
    );

    systemd.tmpfiles.rules = [
      "d ${stateDir} 0750 root root -"
      "d /etc/rustic 0755 root root -"
    ] ++ lib.concatLists (lib.mapAttrsToList (name: t: [
      "d ${targetRoot name} 0750 root root -"
      "d ${targetSrc name} 0750 root root -"
      "d ${targetExtras name} 0750 root root -"
      "L+ /etc/rustic/${name}.toml - - - - ${mkRusticProfile name t}"
    ] ++ lib.optional (t.backendCredentialsFile != null)
      "L+ /etc/rustic/${name}-secrets.toml - - - - ${t.backendCredentialsFile}"
    ) cfg.targets);
  };
}
