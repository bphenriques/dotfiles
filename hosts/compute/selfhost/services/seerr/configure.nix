{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.selfhost.services.seerr;
  radarrCfg = config.selfhost.services.radarr;
  sonarrCfg = config.selfhost.services.sonarr;
  jellyfinCfg = config.selfhost.services.jellyfin;
  mediaCfg = config.custom.media;
  seerrUsers = lib.filterAttrs (_: u: u.services.seerr.enable) config.custom.users;

  initConfig = {
    applicationUrl = serviceCfg.publicUrl;
    jellyfin = {
      hostname = "127.0.0.1";
      inherit (jellyfinCfg) port;
      urlBase = "";
      useSsl = false;
    };
    radarr = {
      name = "Radarr";
      hostname = "127.0.0.1";
      inherit (radarrCfg) port;
      useSsl = false;
      baseUrl = "";
      activeDirectory = config.custom.paths.media.movies;
      activeProfileName = mediaCfg.radarr.profiles.default.name;
      is4k = false;
      minimumAvailability = "released";
      isDefault = true;
      externalUrl = radarrCfg.publicUrl;
    };
    sonarr = {
      name = "Sonarr";
      hostname = "127.0.0.1";
      inherit (sonarrCfg) port;
      useSsl = false;
      baseUrl = "";
      activeDirectory = config.custom.paths.media.tv;
      activeProfileName = mediaCfg.sonarr.profiles.default.name;
      is4k = false;
      isDefault = true;
      externalUrl = sonarrCfg.publicUrl;
    };
    users = lib.mapAttrs (_: user: {
      inherit (user) username;
      inherit (user.services.seerr.permissions) autoApprove advancedRequests viewRecentlyAdded;
    }) seerrUsers;
  };

  initConfigFile = pkgs.writeText "seerr-config.json" (builtins.toJSON initConfig);
in
{
  selfhost.runtimeSecrets = {
    jellyfin-admin-password.restartUnits = [ "seerr-configure.service" ];
    radarr-api-key.restartUnits = [ "seerr-configure.service" ];
    sonarr-api-key.restartUnits = [ "seerr-configure.service" ];
  };

  systemd.services.seerr-configure = {
    description = "Seerr setup";
    wantedBy = [ "seerr.service" ];
    after = [ "seerr.service" "jellyfin-configure.service" "radarr.service" "sonarr.service" ];
    requires = [ "seerr.service" ];
    wants = [ "jellyfin.service" "radarr.service" "sonarr.service" ];
    partOf = [ "seerr.service" "jellyfin-configure.service" ];
    restartTriggers = [ initConfigFile ./configure.nu ];
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
      SEERR_URL = serviceCfg.url;
      SEERR_API_KEY_FILE = config.selfhost.runtimeSecrets.seerr-api-key.path;
      SEERR_CONFIG_FILE = initConfigFile;
      JELLYFIN_ADMIN_USERNAME_FILE = pkgs.writeText "jellyfin-admin-username" "admin";
      JELLYFIN_ADMIN_PASSWORD_FILE = config.selfhost.runtimeSecrets.jellyfin-admin-password.path;
      RADARR_API_KEY_FILE = config.selfhost.runtimeSecrets.radarr-api-key.path;
      SONARR_API_KEY_FILE = config.selfhost.runtimeSecrets.sonarr-api-key.path;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "seerr-configure" ./configure.nu}'';
  };

  assertions = [
    {
      # FIXME: Remove once Seerr supports OIDC
      assertion = lib.all (u: u.services.jellyfin.passwordFile != null) (lib.attrValues seerrUsers);
      message = "All Seerr users must have a Jellyfin passwordFile until Seerr supports OIDC.";
    }
  ];
}
