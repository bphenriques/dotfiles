{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyseerr;
  radarrCfg = config.custom.homelab.services.radarr;
  sonarrCfg = config.custom.homelab.services.sonarr;
  jellyfinCfg = config.custom.homelab.services.jellyfin;
  mediaCfg = config.custom.homelab.media;

  initConfig = {
    applicationUrl = serviceCfg.publicUrl;
    jellyfin = {
      hostname = "127.0.0.1";
      port = jellyfinCfg.port;
      urlBase = "";
      useSsl = false;
    };
    radarr = {
      name = "Radarr";
      hostname = "127.0.0.1";
      port = radarrCfg.port;
      useSsl = false;
      baseUrl = "";
      activeDirectory = config.custom.homelab.paths.media.movies;
      activeProfileName = mediaCfg.radarr.profiles.${mediaCfg.radarr.defaultProfile}.name;
      is4k = false;
      minimumAvailability = "released";
      isDefault = true;
      externalUrl = radarrCfg.publicUrl;
    };
    sonarr = {
      name = "Sonarr";
      hostname = "127.0.0.1";
      port = sonarrCfg.port;
      useSsl = false;
      baseUrl = "";
      activeDirectory = config.custom.homelab.paths.media.tv;
      activeProfileName = mediaCfg.sonarr.profiles.${mediaCfg.sonarr.defaultProfile}.name;
      is4k = false;
      isDefault = true;
      externalUrl = sonarrCfg.publicUrl;
    };
    users = lib.mapAttrs (_: user: {
      inherit (user) username;
      inherit (user.services.jellyseerr.permissions) autoApprove advancedRequests viewRecentlyAdded;
    }) config.custom.homelab.enabledUsers.jellyseerr;
  };

  initConfigFile = pkgs.writeText "jellyseerr-config.json" (builtins.toJSON initConfig);
in
{
  # Cross-service: extend other services' secrets to include this service as dependent
  custom.homelab.services.jellyfin.secrets.systemd.dependentServices = [ "jellyseerr-configure" ];
  custom.homelab.services.radarr.secrets.systemd.dependentServices = [ "jellyseerr-configure" ];
  custom.homelab.services.sonarr.secrets.systemd.dependentServices = [ "jellyseerr-configure" ];

  systemd.services.jellyseerr-configure = {
    description = "Jellyseerr setup";
    wantedBy = [ "multi-user.target" ];
    after = [ "jellyseerr.service" "jellyfin.service" "radarr.service" "sonarr.service" ];
    requires = [ "jellyseerr.service" ];
    wants = [ "jellyfin.service" "radarr.service" "sonarr.service" ];
    partOf = [ "jellyseerr.service" "jellyfin-configure.service" ];
    restartTriggers = [ initConfigFile ./jellyseerr-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      JELLYSEERR_URL = serviceCfg.url;
      JELLYSEERR_API_KEY_FILE = serviceCfg.secrets.files.api-key.path;
      JELLYSEERR_CONFIG_FILE = initConfigFile;
      JELLYFIN_ADMIN_USERNAME_FILE = "${jellyfinCfg.secrets.secretsDir}/admin-username";
      JELLYFIN_ADMIN_PASSWORD_FILE = jellyfinCfg.secrets.files.admin-password.path;
      RADARR_API_KEY_FILE = radarrCfg.secrets.files.api-key.path;
      SONARR_API_KEY_FILE = sonarrCfg.secrets.files.api-key.path;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "jellyseerr-configure" ./jellyseerr-configure.nu}'';
  };
}
