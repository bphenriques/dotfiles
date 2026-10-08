{ config, ... }:
let
  serviceCfg = config.selfhost.services.navidrome;
  sharesCfg = config.fleet.shares;
in
{
  selfhost.services.navidrome = {
    displayName = "Navidrome";
    meta.homepage = "https://www.navidrome.org";
    meta.description = "Music server and streamer";
    meta.category = "media";
    port = 4533;
    access.model = "native";
    storage.mounts = [ "media" ];
    storage.users = [ config.services.navidrome.user ];
    extraConfig.landingPage.enable = true;
  };

  services.navidrome = {
    enable = true;
    openFirewall = false;
    settings = {
      Address = "127.0.0.1";
      Port = serviceCfg.port;
      MusicFolder = "${sharesCfg.media.root}/music/library";
      EnableInsightsCollector = false;
      Scanner.Schedule = "@every 24h";  # CIFS has no inotify
    };
  };
}
