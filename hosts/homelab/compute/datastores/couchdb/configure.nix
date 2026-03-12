{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.couchdb;

  enabledUsers = config.custom.homelab.enabledUsers.couchdb;
  settings = {
    users = lib.mapAttrsToList (_: u: {
      name = u.username;
      passwordFile = config.sops.secrets."couchdb/${u.username}/password".path;
    }) enabledUsers;

    databases = lib.concatLists (lib.mapAttrsToList (_: u:
      map (db: { name = db; owner = u.username; }) u.services.couchdb.databases
    ) enabledUsers);
  };
in
{
  sops.secrets = lib.mapAttrs' (_: u: lib.nameValuePair
    "couchdb/${u.username}/password"
    { group = config.services.couchdb.group; mode = "0440"; } # owner + group read
  ) enabledUsers;

  systemd.services.couchdb-configure = {
    description = "CouchDB setup";
    wantedBy = [ "multi-user.target" ];
    after = [ "couchdb.service" ];
    requires = [ "couchdb.service" ];
    partOf = [ "couchdb.service" ];
    restartTriggers = [ ./couchdb-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      User = config.services.couchdb.user;    # Ensure it is not root.
      Group = config.services.couchdb.group;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      COUCHDB_URL = serviceCfg.url;
      COUCHDB_BIND_ADDRESS = config.services.couchdb.bindAddress;
      COUCHDB_PORT = toString config.services.couchdb.port;
      COUCHDB_ADMIN_USER = config.services.couchdb.adminUser;
      COUCHDB_ADMIN_PASS_FILE = serviceCfg.secrets.files.admin-password.path;
      COUCHDB_SETTINGS_FILE = pkgs.writeText "couchdb-settings.json" (builtins.toJSON settings);
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "couchdb-configure" ./couchdb-configure.nu}'';
  };
}
