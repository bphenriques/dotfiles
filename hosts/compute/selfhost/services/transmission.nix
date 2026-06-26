{ config, ... }:
let
  pathsCfg = config.custom.paths;
  selfhostMounts = config.selfhost.storage.smb.mounts;
in
{
  selfhost.apps.transmission.enable = true;

  selfhost.services.transmission = {
    storage.smb = [ "media" ];
    integrations.notify.topic = "download"; # our taxonomy; the app defaults to "downloads" when present
  };

  # Deployment specifics the app doesn't proxy: where downloads land, seeding policy, the storage backing.
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

  users.users.${config.services.transmission.user}.extraGroups = [ selfhostMounts.media.group ];
}
