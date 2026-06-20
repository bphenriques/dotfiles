{ config, pkgs, lib, self, ... }:
let
  filebrowserDb = config.services.filebrowser.settings.database;
  enabledUsers = lib.filterAttrs (_: u: u.services.filebrowser.enable) config.selfhost.users;

  configFile = self.lib.builders.mkFilebrowserConfig {
    defaults = {
      scope = "/shared";
      permissions = {
        create = true;
        delete = true;
        rename = true;
        modify = true;
        execute = false;
        share = false;
        download = true;
      };
    };
    users = lib.mapAttrsToList (_: u: {
      inherit (u) username;
      inherit (u.services.filebrowser) scope admin;
    } // lib.optionalAttrs (u.services.filebrowser.permissions != null) {
      inherit (u.services.filebrowser) permissions;
    }) enabledUsers;
  };
in
{
  # One-shot setup: configures proxy auth and per-user scopes.
  # Runs *before* filebrowser starts to avoid sqlite locking and a brief window with wrong auth mode.
  #
  # Proxy auth (Remote-User): FileBrowser auto-creates users on first login.
  # Default scope is /shared (guests see only the shared folder).
  # Scopes are relative to --root, not the host filesystem.
  systemd.services.filebrowser-configure = {
    description = "FileBrowser setup";
    requiredBy = [ "filebrowser.service" ];
    before = [ "filebrowser.service" ];
    restartTriggers = [ configFile pkgs.filebrowser-configure ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
      User = "filebrowser";
      Group = "filebrowser";
      StateDirectory = "filebrowser";
    };
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    environment = {
      FILEBROWSER_CONFIG_FILE = configFile;
      FILEBROWSER_DB = filebrowserDb;
      FILEBROWSER_ROOT = config.services.filebrowser.settings.root;
    };
    script = lib.getExe pkgs.filebrowser-configure;
  };
}
