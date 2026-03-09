{ config, pkgs, lib, ... }:
let
  appDataDir = "/var/lib/recyclarr";
  radarrCfg = config.custom.homelab.services.radarr;
  sonarrCfg = config.custom.homelab.services.sonarr;
  secretsFile = "${appDataDir}/secrets.yml";

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
    # TODO: Use the templating.
    preStart = ''
      # Generate secrets.yml from API key files
      cat > "${secretsFile}" <<EOF
movies_api_key: $(cat "${radarrCfg.secrets.files.api-key.path}")
tv_api_key: $(cat "${sonarrCfg.secrets.files.api-key.path}")
EOF
      chmod 600 "${secretsFile}"
    '';
  };

  systemd.timers.recyclarr = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 03:00:00"; # Run daily at 3 AM
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };

  # Cross-service dependencies
  custom.homelab.services.radarr.secrets.systemd.dependentServices = [ "recyclarr" ];
  custom.homelab.services.sonarr.secrets.systemd.dependentServices = [ "recyclarr" ];
}
