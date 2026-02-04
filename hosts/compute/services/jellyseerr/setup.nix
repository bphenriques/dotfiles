{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.routes.jellyseerr;
  radarrCfg = config.custom.home-server.routes.radarr;
  sonarrCfg = config.custom.home-server.routes.sonarr;
  jellyfinCfg = config.custom.home-server.routes.jellyfin;
  mediaCfg = config.custom.home-server.media;

  # Settings to pre-seed settings.json and skip the setup wizard
  # Only __API_KEY__ and __JELLYFIN_API_KEY__ are injected at runtime
  settingsJson = {
    clientId = builtins.hashString "sha256" "jellyseerr-${config.networking.hostName}";
    vapidPrivate = "";
    vapidPublic = "";
    main = {
      apiKey = "__API_KEY__";
      applicationTitle = "Jellyseerr";
      applicationUrl = serviceCfg.publicUrl;
      localLogin = false;
      mediaServerLogin = true;
      mediaServerType = 2; # Jellyfin
      partialRequestsEnabled = true;
      cacheImages = false;
      locale = "en";
    };
    jellyfin = {
      name = "Jellyfin";
      ip = "127.0.0.1";
      port = jellyfinCfg.port;
      useSsl = false;
      urlBase = "";
      serverId = mediaCfg.jellyfin.serverId;
      externalHostname = config.custom.home-server.routes.jellyfin.publicUrl;
      apiKey = "__JELLYFIN_API_KEY__";
    };
    public = {
      initialized = true;
    };
    radarr = [ ];
    sonarr = [ ];
  };

  # Config passed to jellyseerr-init.nu for Radarr/Sonarr registration
  initConfig = {
    jellyfin = {
      url = jellyfinCfg.internalUrl;
    };
    radarr = {
      name = "Radarr";
      hostname = "127.0.0.1";
      port = radarrCfg.port;
      useSsl = false;
      baseUrl = "";
      activeDirectory = config.custom.paths.media.movies;
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
      activeDirectory = config.custom.paths.media.tv;
      activeProfileName = mediaCfg.sonarr.profiles.${mediaCfg.sonarr.defaultProfile}.name;
      is4k = false;
      isDefault = true;
      externalUrl = sonarrCfg.publicUrl;
    };
  };

  settingsFile = pkgs.writeText "jellyseerr-settings.json" (builtins.toJSON settingsJson);
in
{
  sops.secrets."jellyseerr/api-key" = { };

  systemd.services.jellyseerr-init = {
    description = "Initialize Jellyseerr with declarative configuration";
    wantedBy = [ "multi-user.target" ];
    after = [ "jellyfin-init.service" "jellyseerr.service" "radarr-init.service" "sonarr-init.service" ];
    requires = [ "jellyseerr.service" ];
    wants = [ "jellyfin-init.service" "radarr-init.service" "sonarr-init.service" ];
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      JELLYSEERR_URL = serviceCfg.internalUrl;
      JELLYSEERR_SETTINGS_TEMPLATE = settingsFile;
      JELLYSEERR_API_KEY_FILE = config.sops.secrets."jellyseerr/api-key".path;
      JELLYSEERR_CONFIG_FILE = pkgs.writeText "jellyseerr-config.json" (builtins.toJSON initConfig);
      JELLYFIN_ADMIN_USERNAME_FILE = config.sops.secrets."jellyfin/admin/username".path;
      JELLYFIN_ADMIN_PASSWORD_FILE = config.sops.secrets."jellyfin/admin/password".path;
      RADARR_API_KEY_FILE = config.sops.secrets."radarr/api-key".path;
      SONARR_API_KEY_FILE = config.sops.secrets."sonarr/api-key".path;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "jellyseerr-init" ./jellyseerr-init.nu}'';
  };
}
