{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.routes.sonarr;
  mediaCfg = config.custom.home-server.media.sonarr;

  settings = {
    rootFolders = [ { path = config.custom.paths.media.tv; } ];
    downloadClient = {
      name = "Transmission";
      host = "127.0.0.1";
      port = config.custom.home-server.routes.transmission.port;
      urlBase = "/transmission/";
      category = "sonarr";
    };
    # Default quality profile for new shows (set on root folders)
    # Profile name from centralized media settings
    defaultQualityProfile = mediaCfg.profiles.${mediaCfg.defaultProfile}.name;
  };
in
{
  config = lib.mkIf config.services.sonarr.enable {
    systemd.services.sonarr-init = {
      description = "Initialize Sonarr with declarative configuration";
      wantedBy = [ "multi-user.target" ];
      after = [ "sonarr.service" "transmission.service" "recyclarr.service" ];
      requires = [ "sonarr.service" "recyclarr.service" ];
      wants = [ "transmission.service" "recyclarr.service" ];
      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        RestartSec = 10;
        StartLimitBurst = 3;
      };
      environment = {
        SONARR_URL = serviceCfg.internalUrl;
        SONARR_API_KEY_FILE = config.sops.secrets."sonarr/api-key".path;
        SONARR_CONFIG_FILE = pkgs.writeText "sonarr-config.json" (builtins.toJSON settings);
      };
      path = [ pkgs.nushell ];
      script = ''nu ${self.lib.builders.writeNushellScript "sonarr-init" ./sonarr-init.nu}'';
    };
  };
}
