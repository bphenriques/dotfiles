# Pocket-ID post-start configuration
#
# Provisions users and groups in Pocket-ID. New users are invited via email.
# OIDC clients are now provisioned by the compute host directly via API.

{ config, pkgs, lib, self, ... }:
let
  settings = builtins.toJSON rec {
    users = lib.mapAttrsToList (name: u: {
      username = u.username;
      email = u.email;
      firstName = u.firstName;
      lastName = u.lastName;
      isAdmin = u.isAdmin;
      name = "${u.firstName} ${u.lastName}";
      groups = u.groups or [ "users" ];
    }) self.settings.users;

    groups = map (name: { inherit name; }) (lib.unique (lib.concatLists (map (u: u.groups) users)));
  };
in
{
  config = lib.mkIf config.services.pocket-id.enable {
    systemd.services.pocket-id-configure = {
      description = "Configure Pocket ID users and groups";
      wantedBy = [ "multi-user.target" ];
      after = [ "pocket-id.service" ];
      requires = [ "pocket-id.service" ];
      partOf = [ "pocket-id.service" ];
      restartTriggers = [ settings ];
      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        RestartSec = 10;
        StartLimitBurst = 3;
      };
      environment = {
        POCKET_ID_URL = "http://127.0.0.1:${config.services.pocket-id.settings.PORT}";
        POCKET_ID_API_KEY_FILE = config.sops.templates.pocket-id-api-key.path;
        POCKET_ID_CONFIG_FILE = pkgs.writeText "pocket-id-config.json" settings;
      };
      path = [ pkgs.nushell ];
      script = ''nu ${self.lib.builders.writeNushellScript "pocket-id-configure" ./pocket-id-configure.nu}'';
    };
  };
}
