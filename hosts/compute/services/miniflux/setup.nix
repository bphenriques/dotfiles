{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.routes.miniflux;
  initScript = self.lib.builders.writeNushellScript "miniflux-init" ./miniflux-init.nu;

  # Collect users with miniflux settings enabled
  enabledUsers = lib.filterAttrs (_: u: u.services.miniflux.enable) config.custom.home-server.users;
  userSettings = lib.mapAttrsToList (_: u:
    { username = u.username; } // u.services.miniflux.settings
  ) enabledUsers;
in
{
  config = lib.mkIf config.services.miniflux.enable {
    # Enable admin for API access (required for declarative settings)
    services.miniflux = {
      adminCredentialsFile = config.sops.templates."miniflux-admin-credentials".path;
      config = {
        DISABLE_LOCAL_AUTH = 0;
        CREATE_ADMIN = true;
      };
    };

    # Admin credentials for API access
    sops.secrets."miniflux/admin/username" = { };
    sops.secrets."miniflux/admin/password" = { };
    sops.templates."miniflux-admin-credentials" = {
      content = ''
        ADMIN_USERNAME=${config.sops.placeholder."miniflux/admin/username"}
        ADMIN_PASSWORD=${config.sops.placeholder."miniflux/admin/password"}
      '';
    };

    # Initialize user settings after Miniflux starts (re-runs when miniflux restarts)
    systemd.services.miniflux-init = {
      description = "Initialize Miniflux user settings";
      wantedBy = [ "miniflux.service" ];
      after = [ "miniflux.service" ];
      requires = [ "miniflux.service" ];
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
        MINIFLUX_USER_SETTINGS_FILE = pkgs.writeText "miniflux-user-settings.json" (builtins.toJSON userSettings);
      };
      path = [ pkgs.nushell ];
      script = ''nu ${initScript}'';
    };
  };
}
