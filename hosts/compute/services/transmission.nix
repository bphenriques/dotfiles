{ config, pkgs, ... }:
let
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  custom.home-server.routes.transmission.port = 9091;
  services.transmission = {
    enable = true;
    settings = {
      download-dir = pathsCfg.media.downloads.root;
      incomplete_dir_enabled = true;
      incomplete-dir = pathsCfg.media.downloads.incomplete;
      rpc-port = config.custom.home-server.routes.transmission.port;
      rpc_url = config.custom.home-server.routes.transmission.publicUrl;
      idle_seeding_limit_enabled = true;
      idle_seeding_limit = 1; # 1 minute
      # TODO: password
    };

    webHome = pkgs.flood-for-transmission;
  };

  # Ensure we deal correctly with the remote storage dependencies: permissions and service dependency.
  users.users.${config.services.transmission.user}.extraGroups = [ homelabMounts.media.group ];
  systemd.services.transmission = {
    requires = [ homelabMounts.media.automountUnit ];
    after = [ homelabMounts.media.automountUnit ];
  };
}