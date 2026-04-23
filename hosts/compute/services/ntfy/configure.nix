{ config, lib, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.ntfy;
  homelabCfg = config.custom.homelab;
  inherit (homelabCfg.ntfy) topics;

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

  configFile = pkgs.writeText "ntfy-configure.json" (builtins.toJSON {
    publicTopics = lib.attrNames (lib.filterAttrs (_: t: t.public) topics);
    publishers = allPublishers;
  });
in
{
  config = {
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
        TimeoutStartSec = 600;
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
