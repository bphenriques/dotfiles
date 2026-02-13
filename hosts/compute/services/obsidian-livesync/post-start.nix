{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.services.obsidian-livesync;

  enabledUsers = config.custom.home-server.enabledUsers.obsidian-livesync;
  settings = {
    users = lib.mapAttrsToList (_: u: {
      name = u.username;
      passwordFile = config.sops.secrets."obsidian-livesync/${u.username}/password".path;
    }) enabledUsers;

    databases = lib.concatLists (lib.mapAttrsToList (_: u:
      map (db: { name = db; owner = u.username; }) u.services.obsidian-livesync.databases
    ) enabledUsers);
  };
in
{
  # Automatically configure sops secrets for each enabled user
  sops.secrets = lib.mapAttrs' (_: u: lib.nameValuePair
    "obsidian-livesync/${u.username}/password"
    { group = config.services.couchdb.group; mode = "0440"; } # owner + group read
  ) enabledUsers;

  systemd.services.couchdb-configure = {
    description = "Configure CouchDB databases and users";
    wantedBy = [ "multi-user.target" ];
    after = [ "couchdb.service" ];
    requires = [ "couchdb.service" ];
    partOf = [ "couchdb.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = config.services.couchdb.user;    # Ensure it is not root.
      Group = config.services.couchdb.group;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      COUCHDB_URL = serviceCfg.internalUrl;
      COUCHDB_BIND_ADDRESS = config.services.couchdb.bindAddress;
      COUCHDB_PORT = toString config.services.couchdb.port;
      COUCHDB_ADMIN_USER = config.services.couchdb.adminUser;
      COUCHDB_ADMIN_PASS_FILE = config.sops.templates."couchdb-admin-password".path;
      COUCHDB_SETTINGS_FILE = pkgs.writeText "couchdb-settings.json" (builtins.toJSON settings);
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "couchdb-configure" ./couchdb-configure.nu}'';
  };
}
