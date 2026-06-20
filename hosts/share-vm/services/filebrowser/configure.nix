# Seed FileBrowser proxy-auth users every boot against the ephemeral DB, so it both
# adds current users and (by starting fresh) drops any removed from `users`. Uses the
# shared mkFilebrowserConfig builder (same JSON contract as compute's FileBrowser).
{ pkgs, lib, self, ... }:
let
  inherit (import ../../lib.nix { inherit lib; }) filesRoot fbDb fbUsers;

  # Deny-by-default scope; each user opts into rw via its own permissions below.
  fbConfig = self.lib.builders.mkFilebrowserConfig {
    defaults = {
      scope = "/";
      permissions = { create = false; delete = false; rename = false; modify = false; execute = false; share = false; download = false; };
    };
    branding.files = "${../../branding}"; # custom logo (branding/img/logo.svg)
    users = map (u: {
      username = u.name;
      inherit (u) scope;
      admin = false;
      permissions = {
        create = !u.readOnly; delete = !u.readOnly; rename = !u.readOnly; modify = !u.readOnly;
        execute = false; share = false; download = true;
      };
    }) fbUsers;
  };
in
{
  systemd.services.filebrowser-configure = {
    description = "Seed FileBrowser proxy-auth users";
    requiredBy = [ "filebrowser.service" ];
    before = [ "filebrowser.service" ];
    after = [ "systemd-tmpfiles-setup.service" ];
    unitConfig.RequiresMountsFor = [ filesRoot ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "filebrowser";
      Group = "filebrowser";
    };
    environment = {
      FILEBROWSER_CONFIG_FILE = fbConfig;
      FILEBROWSER_DB = fbDb;
      FILEBROWSER_ROOT = filesRoot;
    };
    script = lib.getExe pkgs.filebrowser-configure;
  };
}
