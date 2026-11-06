{ config, lib, ... }:
let
  inherit (config.selfhost) apps services;

  # Build v8 guide-backed quality profiles from media settings
  mkQualityProfiles = mediaCfg:
    map (profile: {
      inherit (profile) name;
      trash_id = profile.trashId;
      reset_unmatched_scores.enabled = true;
    }) (lib.attrValues mediaCfg.profiles);

  mkServiceConfig = mediaCfg: serviceCfg: apiKeyFile: {
    base_url = serviceCfg.url;
    api_key._secret = apiKeyFile;
    quality_definition.type = mediaCfg.qualityDefinitionType;
    quality_profiles = mkQualityProfiles mediaCfg;
  };
in
{
  services.recyclarr = {
    enable = true;
    schedule = "*-*-* 02:00:00"; # before the 03:00 backup, so the arr-configure restarts do not race it
    configuration = {
      radarr.movies = mkServiceConfig config.my.media.radarr services.radarr apps.radarr.apiKeyFile;
      sonarr.tv = mkServiceConfig config.my.media.sonarr services.sonarr apps.sonarr.apiKeyFile;
    };
  };

  selfhost.runtimeSecrets = {
    radarr-api-key.restartUnits = [ "recyclarr.service" ];
    sonarr-api-key.restartUnits = [ "recyclarr.service" ];
  };

  # Also on deploy, after the configure units: they wait for the APIs, the arr units are Type=simple.
  systemd.services.recyclarr = {
    wantedBy = [ "multi-user.target" ];
    after = [ "radarr-configure.service" "sonarr-configure.service" ];
    wants = [ "radarr-configure.service" "sonarr-configure.service" ];
    serviceConfig = {
      # The configure units seed root folders with profiles that only exist once recyclarr has run.
      ExecStartPost = "+${config.systemd.package}/bin/systemctl restart --no-block radarr-configure sonarr-configure";
      StateDirectoryMode = "0700";   # upstream renders the API keys into a world-readable config.yml
    };
  };
}
