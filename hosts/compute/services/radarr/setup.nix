{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.routes.radarr;
  mediaCfg = config.custom.home-server.media.radarr;

  settings = {
    rootFolders = [ { path = config.custom.paths.media.movies; } ];
    downloadClient = {
      name = "Transmission";
      host = "127.0.0.1";
      port = config.custom.home-server.routes.transmission.port;
      urlBase = "/transmission/";
      category = "radarr";
    };
    # Default quality profile for new movies (set on root folders)
    # Profile name from centralized media settings
    defaultQualityProfile = mediaCfg.profiles.${mediaCfg.defaultProfile}.name;
  };
in
{
  systemd.services.radarr-init = {
    description = "Initialize Radarr with declarative configuration";
    wantedBy = [ "multi-user.target" ];
    after = [ "radarr.service" "transmission.service" "recyclarr.service" ];
    requires = [ "radarr.service" "recyclarr.service" ];
    wants = [ "transmission.service" "recyclarr.service" ];
    restartTriggers = [ (builtins.toJSON settings) ];
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      RADARR_URL = serviceCfg.internalUrl;
      RADARR_API_KEY_FILE = config.sops.secrets."radarr/api-key".path;
      RADARR_CONFIG_FILE = pkgs.writeText "radarr-config.json" (builtins.toJSON settings);
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "radarr-init" ./radarr-init.nu}'';
  };
}
