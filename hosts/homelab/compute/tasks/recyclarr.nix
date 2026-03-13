{ config, pkgs, lib, ... }:
let
  appDataDir = "/var/lib/recyclarr";
  radarrCfg = config.custom.homelab.services.radarr;
  sonarrCfg = config.custom.homelab.services.sonarr;

  # Build recyclarr include templates from media settings
  mkIncludeTemplates = serviceCfg:
    [{ template = serviceCfg.qualityDefinitionTemplate; }]
      ++ lib.concatMap (profile: map (t: { template = t; }) profile.recyclarrTemplates) (lib.attrValues serviceCfg.profiles);

  recyclarrConfig = {
    radarr.movies = {
      base_url = radarrCfg.url;
      include = mkIncludeTemplates config.custom.homelab.media.radarr;
    };
    sonarr.tv = {
      base_url = sonarrCfg.url;
      include = mkIncludeTemplates config.custom.homelab.media.sonarr;
    };
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

  # Recyclarr runs as a systemd timer to periodically sync TRaSH guides
  systemd.services.recyclarr = {
    description = "Recyclarr sync";
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
      RECYCLARR_APP_DATA = appDataDir;
    };
  };

  systemd.timers.recyclarr = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 03:00:00"; # Run daily at 3 AM
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };
}
