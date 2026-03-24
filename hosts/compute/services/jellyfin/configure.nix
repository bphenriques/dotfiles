{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyfin;
  pathsCfg = config.custom.homelab.paths;
  oidcCfg = config.custom.homelab.oidc;
  enabledUsers = lib.filterAttrs (_: u: u.services.jellyfin.enable) config.custom.homelab.users;

  jellyfinConfig = {
    serverName = "Jellyfin";
    libraries = [
      { Name = "Movies";    CollectionType = "movies";  Locations = [ pathsCfg.media.movies ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; EnableChapterImageExtraction = true; }
      { Name = "TV Shows";  CollectionType = "tvshows"; Locations = [ pathsCfg.media.tv ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; EnableChapterImageExtraction = true; }
      { Name = "Music";     CollectionType = "music";   Locations = [ pathsCfg.media.music.library ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = false; EnableChapterImageExtraction = false; }
    ];
    # Encoding / transcoding settings (merged into /System/Configuration/encoding via API)
    encodingConfig = {
      HardwareAccelerationType = "qsv";
      EnableHwAcceleration = true;
      EnableHwEncoding = true;
      EnableHwDecoding = true;

      # N150 (Alder Lake-N) only has the low-power fixed-function encoder. These must be enabled.
      EnableIntelLowPowerH264HwEncoder = true;
      EnableIntelLowPowerHevcHwEncoder = true;

      # HDR -> SDR tonemapping via OpenCL (requires intel-compute-runtime in hardware.graphics.extraPackages)
      EnableTonemapping = true;
      TonemappingAlgorithm = "bt2390";
    };
    # Trickplay settings (merged into /System/Configuration TrickplayOptions via API)
    trickplayOptions = {
      EnableHwAcceleration = true;
      EnableHwEncoding = true;
      ScanBehavior = "NonBlocking";
      ProcessPriority = "BelowNormal";
    };
    userConfigs = lib.mapAttrsToList (_: u: {
      username = u.username;
      policy = {
        IsHidden = false;
        EnableSubtitleManagement = true;
      };
    } // lib.optionalAttrs (u.services.jellyfin ? passwordFile) {
      passwordFile = u.services.jellyfin.passwordFile;
    }) enabledUsers;
  };

  jellyfinConfigFile = pkgs.writeText "jellyfin-config.json" (builtins.toJSON jellyfinConfig);
in
{
  custom.homelab.media.jellyfin.serverId = jellyfinConfig.serverName;

  systemd.services.jellyfin-configure = {
    description = "Jellyfin setup";
    wantedBy = [ "jellyfin.service" ];
    after = [ "jellyfin.service" ];
    requires = [ "jellyfin.service" ];
    partOf = [ "jellyfin.service" ];
    restartTriggers = [ jellyfinConfigFile ./jellyfin-configure.nu ];
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
      JELLYFIN_URL = serviceCfg.url;
      JELLYFIN_ADMIN_USERNAME_FILE = pkgs.writeText "jellyfin-admin-username" "admin";  # Not a secret but keeps consistency.
      JELLYFIN_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
      JELLYFIN_CONFIG_FILE = jellyfinConfigFile;
      OIDC_USERS_FILE = oidcCfg.credentials.usersFile; # Validates non-local users exist in OIDC provider
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "jellyfin-configure" ./jellyfin-configure.nu}'';
  };
}
