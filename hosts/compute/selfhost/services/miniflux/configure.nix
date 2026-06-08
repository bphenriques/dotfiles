{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.selfhost.services.miniflux;
  oidcCfg = config.selfhost.auth.oidc;
  enabledUsers = lib.filterAttrs (_: u: u.services.miniflux.enable) config.selfhost.users;

  adminUsernameFile = pkgs.writeText "miniflux-admin-username" "admin";

  userSettingsFile = pkgs.writeText "miniflux-user-settings.json" (builtins.toJSON (lib.mapAttrsToList (_: u:
    { inherit (u) username; } // u.services.miniflux.settings // { is_admin = u.isAdmin; }
  ) enabledUsers));
in
{
  selfhost = {
    runtimeSecrets.miniflux-admin-password = {
      regenerateIfMissing = false;
      restartUnits = [ "miniflux-configure.service" ];
    };

    runtimeTemplates."miniflux-admin-credentials.env" = {
      content = ''
        ADMIN_USERNAME=admin
        ADMIN_PASSWORD=${config.selfhost.runtimePlaceholder.miniflux-admin-password}
      '';
      restartUnits = [ "miniflux.service" ];
    };
  };

  services.miniflux = {
    adminCredentialsFile = config.selfhost.runtimeTemplates."miniflux-admin-credentials.env".path;
    config = {
      DISABLE_LOCAL_AUTH = 0;
      CREATE_ADMIN = true;
    };
  };

  systemd.services.miniflux-configure = {
    description = "Miniflux setup";
    wantedBy = [ "miniflux.service" ];
    after = [ "miniflux.service" ];
    requires = [ "miniflux.service" ];
    partOf = [ "miniflux.service" ];
    restartTriggers = [ userSettingsFile ./miniflux-configure.nu ];
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
      MINIFLUX_URL = serviceCfg.url;
      MINIFLUX_ADMIN_USERNAME_FILE = adminUsernameFile;
      MINIFLUX_ADMIN_PASSWORD_FILE = config.selfhost.runtimeSecrets.miniflux-admin-password.path;
      MINIFLUX_USER_SETTINGS_FILE = userSettingsFile;
      OIDC_USERS_FILE = oidcCfg.credentials.usersFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "miniflux-configure" ./miniflux-configure.nu}'';
  };
}
