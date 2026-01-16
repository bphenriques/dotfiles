{ config, pkgs, ... }:
let
  pathsCfg = config.custom.paths;
in
{
  custom.home-server.services.transmission.port = 9091;

  services.transmission = {
    enable = true;
    settings = {
      download-dir = pathsCfg.media.downloads.root;;
      incomplete_dir_enabled = true;
      incomplete-dir = pathsCfg.media.downloads.incomplete;
      rpc-port = config.custom.home-server.services.transmission.port;
      rpc_url = config.custom.home-server.services.transmission.publicUrl;
      idle_seeding_limit_enabled = true;
      idle_seeding_limit = 1; # 1 minute


      # TODO: To consider
      # https://github.com/transmission/transmission/blob/main/docs/Editing-Configuration-Files.md#rpc
      # Considerations: https://github.com/transmission/transmission/blob/main/docs/Editing-Configuration-Files.md#scheduling
      # rpc_password

      # TODO: notifications?
      # script_torrent_added_enabled
      # script_torrent_added_filename
      # script-torrent-done-enabled = ""; # TODO notify
      # script-torrent-done-filename = "";
    };

    webHome = pkgs.flood-for-transmission;
  };

  # FIXME: Permissions
  # user = "media";
  # group "media";

  #systemd.services.transmission.serviceConfig.BindPaths = [ "/mnt" mediaDir ];
}