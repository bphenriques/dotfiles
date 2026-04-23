{ config, pkgs, lib, ... }:
let
  appDataDir = "/var/lib/recyclarr";
  radarrCfg = config.custom.homelab.services.radarr;
  sonarrCfg = config.custom.homelab.services.sonarr;

  # Build v8 guide-backed quality profiles from media settings
  mkQualityProfiles = mediaCfg:
    map (profile: {
      trash_id = profile.trashId;
      inherit (profile) name;
      reset_unmatched_scores.enabled = true;
    }) (lib.attrValues mediaCfg.profiles);

  mkServiceConfig = mediaCfg: serviceCfg: {
    base_url = serviceCfg.url;
    quality_definition.type = mediaCfg.qualityDefinitionType;
    quality_profiles = mkQualityProfiles mediaCfg;
  };

  recyclarrConfig = {
    radarr.movies = mkServiceConfig config.custom.homelab.media.radarr radarrCfg;
    sonarr.tv = mkServiceConfig config.custom.homelab.media.sonarr sonarrCfg;
  };

  yamlFormat = pkgs.formats.yaml { };
  configFile = yamlFormat.generate "recyclarr.yml" recyclarrConfig;
in
{
  custom.homelab.tasks.recyclarr.secrets = {
    templates."secrets.yml" = {
      content = ''
        movies_api_key: ${radarrCfg.secrets.placeholder.api-key}
        tv_api_key: ${sonarrCfg.secrets.placeholder.api-key}
      '';
      path = "${appDataDir}/secrets.yml";
    };
    systemd.dependentServices = [ "recyclarr" ];
  };

  # Runs once at boot (to apply config on deploy) and daily at 3 AM via timer
  systemd.services.recyclarr = {
    description = "Recyclarr sync";
    wantedBy = [ "multi-user.target" ];
    after = [ "radarr.service" "sonarr.service" ];
    wants = [ "radarr.service" "sonarr.service" ];
    serviceConfig = {
      Type = "oneshot";
      StateDirectory = "recyclarr";
      CacheDirectory = "recyclarr";
      ExecStart = "${pkgs.recyclarr}/bin/recyclarr sync --config ${configFile}";
      ExecStartPost = "+${pkgs.systemd}/bin/systemctl restart --no-block radarr-configure sonarr-configure";

      # Hardening
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictSUIDSGID = true;
    };
    environment = {
      RECYCLARR_CONFIG_DIR = appDataDir;
    };
  };

  systemd.timers.recyclarr = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 02:00:00"; # Run before backup (3 AM) to avoid racing with arr-configure restarts
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };
}
