{ config, lib, ... }:
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
    healthcheck.path = "/healthz";
  };

  systemd.services.navidrome.serviceConfig.ReadOnlyPaths = [
    "${sharesCfg.media.root}/music/library"
  ];

  services.navidrome = {
    enable = true;
    openFirewall = false;
    settings.Address = "127.0.0.1";
    settings.EnableInsightsCollector = false;
  };
}
