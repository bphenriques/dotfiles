{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.services.jellyseerr;
  radarrCfg = config.custom.home-server.services.radarr;
  sonarrCfg = config.custom.home-server.services.sonarr;
  jellyfinCfg = config.custom.home-server.services.jellyfin;
  mediaCfg = config.custom.home-server.media;
  jellyseerrUsers = config.custom.home-server.enabledUsers.jellyseerr;

  initConfigJson = builtins.toJSON initConfig;

  # Config passed to jellyseerr-configure.nu for setup and Radarr/Sonarr registration
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
    # Users with their Jellyseerr permission settings
    users = lib.mapAttrs (_: user: {
      username = user.username;
      autoApprove = user.services.jellyseerr.permissions.autoApprove;
      advancedRequests = user.services.jellyseerr.permissions.advancedRequests;
      viewRecentlyAdded = user.services.jellyseerr.permissions.viewRecentlyAdded;
    }) jellyseerrUsers;
  };
in
{
  systemd.services.jellyseerr-configure = {
    description = "Configure Jellyseerr with declarative configuration";
    wantedBy = [ "multi-user.target" ];
    after = [ "jellyseerr.service" "jellyfin.service" "radarr.service" "sonarr.service" ];
    requires = [ "jellyseerr.service" ];
    wants = [ "jellyfin.service" "radarr.service" "sonarr.service" ];
    partOf = [ "jellyfin-configure.service" ];  # Restart when jellyfin-configure restarts (new users to sync)
    restartTriggers = [ initConfigJson ./jellyseerr-configure.nu ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      JELLYSEERR_URL = serviceCfg.internalUrl;
      JELLYSEERR_API_KEY_FILE = config.sops.secrets."jellyseerr/api-key".path;
      JELLYSEERR_CONFIG_FILE = pkgs.writeText "jellyseerr-config.json" initConfigJson;
      JELLYFIN_ADMIN_USERNAME_FILE = config.sops.secrets."jellyfin/admin/username".path;
      JELLYFIN_ADMIN_PASSWORD_FILE = config.sops.secrets."jellyfin/admin/password".path;
      RADARR_API_KEY_FILE = config.sops.secrets."radarr/api-key".path;
      SONARR_API_KEY_FILE = config.sops.secrets."sonarr/api-key".path;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "jellyseerr-configure" ./jellyseerr-configure.nu}'';
  };
}
