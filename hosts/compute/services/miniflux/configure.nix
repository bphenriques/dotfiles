{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
  oidcCfg = config.custom.homelab.oidc;
  enabledUsers = lib.filterAttrs (_: u: u.services.miniflux.enable) config.custom.homelab.users;

  adminUsernameFile = pkgs.writeText "miniflux-admin-username" "admin";

  userSettingsFile = pkgs.writeText "miniflux-user-settings.json" (builtins.toJSON (lib.mapAttrsToList (_: u:
    { username = u.username; } // u.services.miniflux.settings // { is_admin = u.isAdmin; }
  ) enabledUsers));
in
{
  custom.homelab.services.miniflux.secrets = {
    files.admin-password = { rotatable = false; };
    templates."admin-credentials.env".content = ''
      ADMIN_USERNAME=admin
      ADMIN_PASSWORD=${serviceCfg.secrets.placeholder.admin-password}
    '';
    systemd.dependentServices = [ "miniflux" "miniflux-configure" ];
  };

  services.miniflux = {
    adminCredentialsFile = serviceCfg.secrets.templates."admin-credentials.env".path;
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
      MINIFLUX_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
      MINIFLUX_USER_SETTINGS_FILE = userSettingsFile;
      OIDC_USERS_FILE = oidcCfg.credentials.usersFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "miniflux-configure" ./miniflux-configure.nu}'';
  };
}
