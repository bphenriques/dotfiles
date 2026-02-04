{ config, pkgs, ... }:
let
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  custom.home-server.routes.transmission.port = 9091;
  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
    settings = {
      download-dir = pathsCfg.media.downloads.root;
      incomplete_dir_enabled = true;
      incomplete-dir = pathsCfg.media.downloads.incomplete;
      rpc-port = config.custom.home-server.routes.transmission.port;
      rpc-host-whitelist-enabled = true;
      rpc-host-whitelist = config.custom.home-server.routes.transmission.publicHost;
      ratio-limit-enabled = true;
      ratio-limit = 1;
      idle_seeding_limit_enabled = true;
      idle_seeding_limit = 1; # 1 minute
      umask = 2; # 0002 - Allow group read/write for sonarr/radarr access
    };
    webHome = pkgs.flood-for-transmission;
  };

  # Ensure transmission has the correct permissions to the directory and that it waits for the mounts
  users.users.${config.services.transmission.user}.extraGroups = [ homelabMounts.media.group ];
  systemd.services.transmission = {
    requires = [ homelabMounts.media.automountUnit ];
    after = [ homelabMounts.media.automountUnit ];
  };
}