{ config, pkgs, lib, self, private, ... }:
let
  serviceCfg = config.custom.homelab.services.prowlarr;
  radarrCfg = config.custom.homelab.services.radarr;
  sonarrCfg = config.custom.homelab.services.sonarr;
  ntfyCfg = config.custom.homelab.services.ntfy;

  settings = {
    # Indexers have these fields:
    # - name: Display name in Prowlarr
    # - definitionName: Internal name (find via Prowlarr UI or API: GET /api/v1/indexer/schema)
    # - fields: Optional fields (e.g., baseUrl, apiKey, username/password/cookie). Not required for public indexers.
    inherit (private.settings.services.prowlarr) indexers;
    applications = [
      {
        name = "Radarr";
        implementation = "Radarr";
        syncLevel = "fullSync";
        baseUrl = radarrCfg.url;
        prowlarrUrl = serviceCfg.url;
      }
      {
        name = "Sonarr";
        implementation = "Sonarr";
        syncLevel = "fullSync";
        baseUrl = sonarrCfg.url;
        prowlarrUrl = serviceCfg.url;
      }
    ];
    notification = {
      serverUrl = ntfyCfg.url;
      inherit (serviceCfg.integrations.ntfy) topic;
      tags = "mag";
    };
  };

  settingsFile = pkgs.writeText "prowlarr-config.json" (builtins.toJSON settings);
in
{
  systemd.services.prowlarr-configure = {
    description = "Prowlarr setup";
    wantedBy = [ "prowlarr.service" ];
    after = [ "prowlarr.service" "radarr-configure.service" "sonarr-configure.service" "ntfy-configure.service" ];
    requires = [ "prowlarr.service" ];
    wants = [ "radarr-configure.service" "sonarr-configure.service" "ntfy-configure.service" ];
    partOf = [ "prowlarr.service" ];
    restartTriggers = [ settingsFile ./prowlarr-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      PROWLARR_URL = serviceCfg.url;
      PROWLARR_API_KEY_FILE = serviceCfg.secrets.files.api-key.path;
      PROWLARR_CONFIG_FILE = settingsFile;
      RADARR_API_KEY_FILE = radarrCfg.secrets.files.api-key.path;
      SONARR_API_KEY_FILE = sonarrCfg.secrets.files.api-key.path;
      NTFY_TOKEN_FILE = serviceCfg.integrations.ntfy.tokenFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "prowlarr-configure" ./prowlarr-configure.nu}'';
  };

  # Cross-service dependencies
  custom.homelab.services.radarr.secrets.systemd.dependentServices = [ "prowlarr-configure" ];
  custom.homelab.services.sonarr.secrets.systemd.dependentServices = [ "prowlarr-configure" ];
}
