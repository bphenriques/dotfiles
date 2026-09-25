{ config, ... }:
let
  sharesCfg = config.fleet.shares;
in
{
  selfhost = {
    apps.transmission.enable = true;
    services.transmission = {
      storage.mounts = [ "media" ];
      storage.users = [ config.services.transmission.user ];
      integrations.notify.topic = "download";
      extraConfig.landingPage = { enable = true; listed = false; };
    };
  };

  services.transmission.settings = {
    download-dir = "${sharesCfg.media.root}/downloads";
    incomplete_dir_enabled = true;
    incomplete-dir = "${sharesCfg.media.root}/downloads/incomplete";
    ratio-limit-enabled = true;
    ratio-limit = 1;
    idle_seeding_limit_enabled = true;
    idle_seeding_limit = 60;
    umask = 2; # group-writable downloads for the media group (arr/jellyfin share)
  };

}
