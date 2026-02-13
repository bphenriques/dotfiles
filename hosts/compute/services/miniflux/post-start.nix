{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.services.miniflux;
  oidcCfg = config.custom.home-server.oidc;

  userSettings = builtins.toJSON (lib.mapAttrsToList (_: u:
    { username = u.username; } // u.services.miniflux.settings
  ) config.custom.home-server.enabledUsers.miniflux);
in
{
  services.miniflux = {
    adminCredentialsFile = config.sops.templates."miniflux-admin-credentials".path;
    config = {
      DISABLE_LOCAL_AUTH = 0;
      CREATE_ADMIN = true;
    };
  };
  sops = {
    secrets."miniflux/admin/username" = { };
    secrets."miniflux/admin/password" = { };
    templates."miniflux-admin-credentials" = {
      content = ''
        ADMIN_USERNAME=${config.sops.placeholder."miniflux/admin/username"}
        ADMIN_PASSWORD=${config.sops.placeholder."miniflux/admin/password"}
      '';
    };
  };

  systemd.services.miniflux-configure = {
    description = "Configure Miniflux users and settings";
    wantedBy = [ "miniflux.service" ];
    after = [ "miniflux.service" oidcCfg.systemd.provisionedTarget ];
    requires = [ "miniflux.service" ];
    wants = [ oidcCfg.systemd.provisionedTarget ];
    restartTriggers = [ userSettings ];
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      MINIFLUX_URL = serviceCfg.internalUrl;
      MINIFLUX_ADMIN_USERNAME_FILE = config.sops.secrets."miniflux/admin/username".path;
      MINIFLUX_ADMIN_PASSWORD_FILE = config.sops.secrets."miniflux/admin/password".path;
      MINIFLUX_USER_SETTINGS_FILE = pkgs.writeText "miniflux-user-settings.json" userSettings;
      OIDC_USERS_FILE = oidcCfg.credentials.usersFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "miniflux-configure" ./miniflux-configure.nu}'';
  };
}
