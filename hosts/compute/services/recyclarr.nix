{ config, pkgs, lib, ... }:
let
  appDataDir = "/var/lib/recyclarr";
  secretsGroup = "recyclarr-secrets";

  # Build recyclarr include templates from media settings
  mkIncludeTemplates = serviceCfg:
    [{ template = serviceCfg.qualityDefinitionTemplate; }]
      ++ lib.concatMap (profile: map (t: { template = t; }) profile.recyclarrTemplates) (lib.attrValues serviceCfg.profiles);

  recyclarrConfig = {
    radarr.movies = {
      base_url = config.custom.home-server.routes.radarr.internalUrl;
      include = mkIncludeTemplates config.custom.home-server.media.radarr;
    };
    sonarr.tv = {
      base_url = config.custom.home-server.routes.sonarr.internalUrl;
      include = mkIncludeTemplates config.custom.home-server.media.sonarr;
    };
  };

  yamlFormat = pkgs.formats.yaml { };
  configFile = yamlFormat.generate "recyclarr.yml" recyclarrConfig;
in
{
  users.groups.${secretsGroup} = { };

  sops.templates."recyclarr-secrets.yml" = {
    content = ''
      movies_api_key: ${config.sops.placeholder."radarr/api-key"}
      tv_api_key: ${config.sops.placeholder."sonarr/api-key"}
    '';
    group = secretsGroup;
    mode = "0440";
  };

  # Recyclarr runs as a systemd timer to periodically sync TRaSH guides
  systemd.services.recyclarr = {
    description = "Sync TRaSH guides to Radarr/Sonarr via Recyclarr";
    after = [ "radarr.service" "sonarr.service" ];
    wants = [ "radarr.service" "sonarr.service" ];
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      SupplementaryGroups = [ secretsGroup ];
      StateDirectory = "recyclarr";
      CacheDirectory = "recyclarr";
      ExecStartPre = "${pkgs.coreutils}/bin/ln -sf ${config.sops.templates."recyclarr-secrets.yml".path} ${appDataDir}/secrets.yml";
      ExecStart = "${pkgs.recyclarr}/bin/recyclarr sync --config ${configFile}";
      ExecStartPost = "+${pkgs.systemd}/bin/systemctl restart --no-block radarr-init sonarr-init";

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

  # Run daily at 3 AM
  systemd.timers.recyclarr = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 03:00:00";
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };
}
