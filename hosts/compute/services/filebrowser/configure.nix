{ config, pkgs, self, ... }:
let
  filebrowserDb = config.services.filebrowser.settings.database;

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
    users = [
      { username = "bphenriques"; scope = "/"; admin = true; }
    ];
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
  # bphenriques gets scope / (sees both shared/ and bphenriques/).
  # Scopes are relative to --root, not the host filesystem.
  systemd.services.filebrowser-configure = {
    description = "FileBrowser setup";
    requiredBy = [ "filebrowser.service" ];
    before = [ "filebrowser.service" ];
    restartTriggers = [ configFile ./filebrowser-configure.nu ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "filebrowser";
      Group = "filebrowser";
      StateDirectory = "filebrowser";
    };
    environment = {
      FILEBROWSER_CONFIG_FILE = configFile;
      FILEBROWSER_DB = filebrowserDb;
    };
    path = [ pkgs.filebrowser pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "filebrowser-configure" ./filebrowser-configure.nu}'';
  };
}
