{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.selfhost.services.miniflux;
  oidcCfg = config.selfhost.auth.oidc;

  # Per-user Miniflux personalization is a consumer concern, declared in custom.users (not selfhost-nix).
  settingsUsers = lib.filterAttrs (_: u: (u.selfhost.apps.miniflux or { }) != { }) config.custom.users;

  adminUsernameFile = pkgs.writeText "miniflux-admin-username" "admin";
  userSettingsFile = pkgs.writeText "miniflux-user-settings.json" (
    builtins.toJSON (
      lib.mapAttrsToList (
        _: u: { inherit (u) username; } // u.selfhost.apps.miniflux // { is_admin = u.isAdmin; }
      ) settingsUsers
    )
  );
in
{
  # Re-run when the app's admin password changes (the selfhost-nix app owns the secret itself).
  selfhost.runtimeSecrets.miniflux-admin-password.restartUnits = [ "miniflux-configure.service" ];

  systemd.services.miniflux-configure = {
    description = "Apply per-user Miniflux settings";
    wantedBy = [ "miniflux.service" ];
    after = [ "miniflux.service" ];
    requires = [ "miniflux.service" ];
    partOf = [ "miniflux.service" ];
    restartTriggers = [
      userSettingsFile
      ./miniflux-configure.nu
    ];
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
