{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyfin;
  pathsCfg = config.custom.homelab.paths;
  oidcCfg = config.custom.homelab.oidc;
  enabledUsers = lib.filterAttrs (_: u: u.services.jellyfin.enable) config.custom.homelab.users;

  adminUsername = "admin";
  adminUsernameFile = "${serviceCfg.secrets.secretsDir}/admin-username";

  jellyfinConfig = {
    serverName = "Jellyfin";
    libraries = [
      { Name = "Movies";    CollectionType = "movies";  Locations = [ pathsCfg.media.movies ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; }
      { Name = "TV Shows";  CollectionType = "tvshows"; Locations = [ pathsCfg.media.tv ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = true; }
      { Name = "Music";     CollectionType = "music";   Locations = [ pathsCfg.media.music.library ]; EnableRealtimeMonitor = true; ExtractTrickplayImagesDuringLibraryScan = false; }
    ];
    # Encoding / transcoding settings (merged into /System/Configuration/encoding via API)
    encodingConfig = {
      HardwareAccelerationType = "qsv";
      EnableHwAcceleration = true;
      EnableHwEncoding = true;
      EnableHwDecoding = true;
      # N150 (Alder Lake-N) only has the low-power fixed-function encoder — these must be enabled.
      EnableIntelLowPowerH264HwEncoder = true;
      EnableIntelLowPowerHevcHwEncoder = true;
      # HDR→SDR tonemapping: requires intel-compute-runtime (OpenCL) in hardware.graphics.extraPackages.
      # EnableTonemapping = true;
      # TonemappingAlgorithm = "bt2390";
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
  custom.homelab.media.jellyfin.serverId = jellyfinConfig.serverName; # FIXME: is it serverName?

  systemd.services.jellyfin-configure = {
    description = "Jellyfin setup";
    wantedBy = [ "multi-user.target" ];
    after = [ "jellyfin.service" ];
    requires = [ "jellyfin.service" ];
    partOf = [ "jellyfin.service" ];
    restartTriggers = [ jellyfinConfigFile ./jellyfin-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      JELLYFIN_URL = serviceCfg.url;
      JELLYFIN_ADMIN_USERNAME_FILE = adminUsernameFile;
      JELLYFIN_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
      JELLYFIN_CONFIG_FILE = jellyfinConfigFile;
      OIDC_USERS_FILE = oidcCfg.credentials.usersFile; # Validates non-local users exist in OIDC provider
    };
    path = [ pkgs.nushell ];
    preStart = ''
      # Create admin username file if missing (password is managed by homelab-secrets-jellyfin)
      if [ ! -f "${adminUsernameFile}" ]; then
        echo "${adminUsername}" > "${adminUsernameFile}"
        chown root:${serviceCfg.secrets.group} "${adminUsernameFile}"
        chmod 640 "${adminUsernameFile}"
      fi
    '';
    script = ''nu ${self.lib.builders.writeNushellScript "jellyfin-configure" ./jellyfin-configure.nu}'';
  };
}
