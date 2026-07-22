{ config, ... }:
let
  pathsCfg = config.custom.paths;
in
{
  selfhost = {
    apps.transmission.enable = true;
    services.transmission = {
      storage.smb = [ "media" ];
      integrations.notify.topic = "download";
      extraConfig.landingPage.enable = true;
    };
  };

  services.transmission.settings = {
    download-dir = pathsCfg.media.downloads.root;
    incomplete_dir_enabled = true;
    incomplete-dir = pathsCfg.media.downloads.incomplete;
    ratio-limit-enabled = true;
    ratio-limit = 1;
    idle_seeding_limit_enabled = true;
    idle_seeding_limit = 1;
    umask = 2; # group-writable downloads for the media group (arr/jellyfin share)
  };

  users.users.${config.services.transmission.user}.extraGroups = [ config.selfhost.storage.smb.mounts.media.group ];
}
