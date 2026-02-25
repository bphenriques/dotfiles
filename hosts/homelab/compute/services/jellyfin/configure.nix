{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyfin;
  pathsCfg = config.custom.paths;
  oidcCfg = config.custom.homelab.oidc;

  jellyfinConfigJson = builtins.toJSON jellyfinConfig;
  jellyfinConfig = {
    serverName = "Jellyfin";
    libraries = [
      { Name = "Movies";    CollectionType = "movies";  Locations = [ pathsCfg.media.movies ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; }
      { Name = "TV Shows";  CollectionType = "tvshows"; Locations = [ pathsCfg.media.tv ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; }
      { Name = "Music";     CollectionType = "music";   Locations = [ pathsCfg.media.music.library ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = false; }
    ];
    trickplayConfig = {
      EnableHwAcceleration = true;
      EnableHwEncoding = true;
      EnableHwDecoding = true;
    };
    userConfigs = lib.mapAttrsToList (_: u: {
      username = u.username;
      policy = {
        IsHidden = false;
        EnableSubtitleManagement = true;
      };
    } // lib.optionalAttrs (u.services.jellyfin ? passwordFile) {
      passwordFile = u.services.jellyfin.passwordFile;
    }) config.custom.homelab.enabledUsers.jellyfin;
  };
in
{
  sops = {
    secrets."jellyfin/admin/username" = { };
    secrets."jellyfin/admin/password" = { };
  };

  custom.homelab.media.jellyfin.serverId = jellyfinConfig.serverName; # FIXME: is it serverName?

  systemd.services.jellyfin-configure = {
    description = "Configure Jellyfin with declarative configuration";
    wantedBy = [ "multi-user.target" ];
    after = [ "jellyfin.service" ];
    requires = [ "jellyfin.service" ];
    restartTriggers = [ jellyfinConfigJson ./jellyfin-configure.nu ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      JELLYFIN_URL = serviceCfg.internalUrl;
      JELLYFIN_ADMIN_USERNAME_FILE = config.sops.secrets."jellyfin/admin/username".path;
      JELLYFIN_ADMIN_PASSWORD_FILE = config.sops.secrets."jellyfin/admin/password".path;
      JELLYFIN_CONFIG_FILE = pkgs.writeText "jellyfin-config.json" jellyfinConfigJson;
      OIDC_USERS_FILE = oidcCfg.credentials.usersFile; # FIXME: This has nothing to do really.
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "jellyfin-configure" ./jellyfin-configure.nu}'';
  };
}
