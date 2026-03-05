{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
  oidcCfg = config.custom.homelab.oidc;

  # Credentials file location (derived from secrets)
  adminCredentialsFile = "${serviceCfg.secrets.secretsDir}/admin-credentials.env";
  adminUsernameFile = pkgs.writeText "miniflux-admin-username" "admin";

  userSettingsFile = pkgs.writeText "miniflux-user-settings.json" (builtins.toJSON (lib.mapAttrsToList (_: u:
    { username = u.username; } // u.services.miniflux.settings
  ) config.custom.homelab.enabledUsers.miniflux));
in
{
  custom.homelab.services.miniflux.secrets = {
    files.admin-password = { rotatable = false; };
    systemd.dependentServices = [ "miniflux" "miniflux-configure" ];
  };

  services.miniflux = {
    inherit adminCredentialsFile;
    config = {
      DISABLE_LOCAL_AUTH = 0;
      CREATE_ADMIN = true;
    };
  };

  # Generate credentials file in secrets service (runs before miniflux)
  systemd.services.homelab-secrets-miniflux.serviceConfig.ExecStartPost = pkgs.writeShellScript "generate-miniflux-credentials" ''
    cat > "${adminCredentialsFile}" <<EOF
ADMIN_USERNAME=admin
ADMIN_PASSWORD=$(cat "${serviceCfg.secrets.files.admin-password.path}")
EOF
    chown root:${serviceCfg.secrets.group} "${adminCredentialsFile}"
    chmod 640 "${adminCredentialsFile}"
  '';

  systemd.services.miniflux-configure = {
    description = "Configure Miniflux users and settings";
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
