{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.transmission;
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  custom.homelab.services.transmission = {
    port = 9091;
    forwardAuth.enable = true;
    dashboard = {
      enable = true;
      category = "Media";
      description = "Torrent Client";
      icon = "transmission.svg";
    };
  };
  # https://gethomepage.dev/widgets/services/transmission/

  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
    settings = {
      download-dir = pathsCfg.media.downloads.root;
      incomplete_dir_enabled = true;
      incomplete-dir = pathsCfg.media.downloads.incomplete;
      rpc-port = serviceCfg.port;
      rpc-host-whitelist-enabled = true;
      rpc-host-whitelist = serviceCfg.publicHost;
      ratio-limit-enabled = true;
      ratio-limit = 1;
      idle_seeding_limit_enabled = true;
      idle_seeding_limit = 1;
      umask = 2;
    };
    webHome = pkgs.flood-for-transmission;
  };

  users.users.${config.services.transmission.user}.extraGroups = [ homelabMounts.media.group ];
  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "transmission" ];

  systemd.services.transmission = {
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };
}
