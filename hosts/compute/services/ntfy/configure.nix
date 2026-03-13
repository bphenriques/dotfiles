{ config, lib, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.ntfy;
  homelabCfg = config.custom.homelab;

  topics = {
    media    = { public = true; };
    download = { public = false; };
    admin    = { public = false; };
    backups  = { public = false; };
  };

  # Derive publishers from services and tasks with ntfy integration
  ntfyServices = lib.filterAttrs (_: s: s.integrations.ntfy.enable) homelabCfg.services;
  servicePublishers = lib.mapAttrs (name: s: {
    inherit (s.integrations.ntfy) topic tokenFile;
    owner = name;
  }) ntfyServices;

  ntfyTasks = lib.filterAttrs (_: t: t.integrations.ntfy.enable) homelabCfg.tasks;
  taskPublishers = lib.mapAttrs (name: t: {
    inherit (t.integrations.ntfy) topic tokenFile;
    owner = name;
  }) ntfyTasks;

  allPublishers = servicePublishers // taskPublishers;
  allTopics = lib.unique (lib.mapAttrsToList (_: p: p.topic) allPublishers);

  configFile = pkgs.writeText "ntfy-configure.json" (builtins.toJSON {
    publicTopics = lib.attrNames (lib.filterAttrs (_: t: t.public) topics);
    publishers = allPublishers;
  });
in
{
  config = {
    assertions = let
      unknownTopics = lib.filter (t: !(builtins.hasAttr t topics)) allTopics;
    in [{
      assertion = unknownTopics == [];
      message = "ntfy: unknown topics referenced by publishers: ${toString unknownTopics}. Known: ${toString (lib.attrNames topics)}";
    }];

    custom.homelab.services.ntfy.secrets = {
      files.admin-password = { rotatable = true; };
      systemd.dependentServices = [ "ntfy-sh" "ntfy-configure" ];
    };

    systemd.tmpfiles.rules = [
      "d /var/lib/homelab-secrets/ntfy-publishers 0711 root root -"
    ];

    systemd.services.ntfy-configure = {
      description = "ntfy setup";
      wantedBy = [ "ntfy-sh.service" ];
      after = [ "ntfy-sh.service" ];
      requires = [ "ntfy-sh.service" ];
      partOf = [ "ntfy-sh.service" ];
      restartTriggers = [ configFile ./ntfy-configure.nu ];
      startLimitIntervalSec = 300;
      startLimitBurst = 3;
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        Restart = "on-failure";
        RestartSec = 10;
        UMask = "0077";
      };
      environment = {
        NTFY_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
        NTFY_PROVISION_FILE = configFile;
      };
      path = [ config.services.ntfy-sh.package pkgs.nushell pkgs.coreutils ];
      script = ''nu ${self.lib.builders.writeNushellScript "ntfy-configure" ./ntfy-configure.nu}'';
    };
  };
}
