{ config, pkgs, lib, ... }:
let
  appDataDir = "/var/lib/recyclarr";
  radarrCfg = config.selfhost.services.radarr;
  sonarrCfg = config.selfhost.services.sonarr;

  # Build v8 guide-backed quality profiles from media settings
  mkQualityProfiles = mediaCfg:
    map (profile: {
      inherit (profile) name;
      trash_id = profile.trashId;
      reset_unmatched_scores.enabled = true;
    }) (lib.attrValues mediaCfg.profiles);

  mkServiceConfig = mediaCfg: serviceCfg: {
    base_url = serviceCfg.url;
    quality_definition.type = mediaCfg.qualityDefinitionType;
    quality_profiles = mkQualityProfiles mediaCfg;
  };

  recyclarrConfig = {
    radarr.movies = mkServiceConfig config.custom.media.radarr radarrCfg;
    sonarr.tv = mkServiceConfig config.custom.media.sonarr sonarrCfg;
  };

  yamlFormat = pkgs.formats.yaml { };
  configFile = yamlFormat.generate "recyclarr.yml" recyclarrConfig;
in
{
  # Override path: recyclarr reads from its config dir, not the default /run/homelab-secrets/templates/.
  selfhost.runtimeTemplates."recyclarr-secrets.yml" = {
    content = ''
      movies_api_key: ${config.selfhost.runtimePlaceholder.radarr-api-key}
      tv_api_key: ${config.selfhost.runtimePlaceholder.sonarr-api-key}
    '';
    path = "${appDataDir}/secrets.yml";
    restartUnits = [ "recyclarr.service" ];
  };

  # Runs once at boot (to apply config on deploy) and daily at 2 AM via timer
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
