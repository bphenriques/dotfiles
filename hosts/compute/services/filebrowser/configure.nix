{ config, pkgs, lib, self, ... }:
let
  filebrowserDb = config.services.filebrowser.settings.database;
  enabledUsers = lib.filterAttrs (_: u: u.services.filebrowser.enable) config.custom.homelab.users;

  configFile = pkgs.writeText "filebrowser-configure.json" (builtins.toJSON {
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
      username = u.username;
      scope = u.services.filebrowser.scope;
      admin = u.services.filebrowser.admin;
    } // lib.optionalAttrs (u.services.filebrowser.permissions != null) {
      permissions = u.services.filebrowser.permissions;
    }) enabledUsers;
    branding = {
      name = "Shared Files";
      disableExternal = true;
      disableUsedPercentage = true;
    };
    viewMode = "mosaic";
    singleClick = true;
    hideDotfiles = true;
    sorting = {
      by = "modified";
      asc = false;
    };
  });
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
    restartTriggers = [ configFile ./filebrowser-configure.nu ];
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
    path = [ pkgs.filebrowser pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "filebrowser-configure" ./filebrowser-configure.nu}'';
  };
}
