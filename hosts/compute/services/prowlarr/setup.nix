{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.routes.prowlarr;

  settings = {
    # Indexers have the following fields:
    # - name: Display name in Prowlarr
    # - definitionName: Internal name (find via Prowlarr UI or API: GET /api/v1/indexer/schema)
    # - fields: Optional fields (e.g., baseUrl, apiKey, username/password/cookie). Not required for public indexers.
    indexers = self.settings.services.prowlarr.indexers;
    applications = [
      {
        name = "Radarr";
        implementation = "Radarr";
        syncLevel = "fullSync";
        baseUrl = config.custom.home-server.routes.radarr.internalUrl;
        prowlarrUrl = serviceCfg.internalUrl;
      }
      {
        name = "Sonarr";
        implementation = "Sonarr";
        syncLevel = "fullSync";
        baseUrl = config.custom.home-server.routes.sonarr.internalUrl;
        prowlarrUrl = serviceCfg.internalUrl;
      }
    ];
  };
in
{
  systemd.services.prowlarr-init = {
    description = "Initialize Prowlarr with declarative configuration";
    wantedBy = [ "multi-user.target" ];
    after = [ "prowlarr.service" "radarr-init.service" "sonarr-init.service" ];
    requires = [ "prowlarr.service" ];
    wants = [ "radarr-init.service" "sonarr-init.service" ];
    restartTriggers = [ (builtins.toJSON settings) ];
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      PROWLARR_URL = serviceCfg.internalUrl;
      PROWLARR_API_KEY_FILE = config.sops.secrets."prowlarr/api-key".path;
      PROWLARR_CONFIG_FILE = pkgs.writeText "prowlarr-config.json" (builtins.toJSON settings);
      RADARR_API_KEY_FILE = config.sops.secrets."radarr/api-key".path;
      SONARR_API_KEY_FILE = config.sops.secrets."sonarr/api-key".path;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "prowlarr-init" ./prowlarr-init.nu}'';
  };
}
