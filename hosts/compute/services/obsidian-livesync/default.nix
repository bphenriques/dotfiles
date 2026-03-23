{ lib, config, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.couchdb;

  enabledUsers = lib.filterAttrs (_: u: u.services.couchdb.enable) config.custom.homelab.users;
  settings = {
    users = lib.mapAttrsToList (_: u: {
      name = u.username;
      passwordFile = config.sops.secrets."couchdb/${u.username}/password".path;
    }) enabledUsers;

    databases = lib.concatLists (lib.mapAttrsToList (_: u:
      map (db: { name = db; owner = u.username; }) u.services.couchdb.databases
    ) enabledUsers);
  };

  cors = {
    origins = [ "app://obsidian.md" "capacitor://localhost" "http://localhost" ];
    methods = [ "GET" "PUT" "POST" "HEAD" "DELETE" ];
    headers = [ "accept" "authorization" "content-type" "origin" "referer" ];
    credentials = true;
    maxAge = 3600;
  };

  toCommaSeparated = lib.concatStringsSep ", ";
in
{
  custom.homelab.services.couchdb = {
    displayName = "CouchDB";
    port = 5984;
    metadata.category = "Infrastructure";
    metadata.description = "Document Database";
    metadata.version = config.services.couchdb.package.version;
    metadata.homepage = config.services.couchdb.package.meta.homepage;
    healthcheck.path = "/_up";
    secrets = {
      files.admin-password = { rotatable = false; };
      templates."admin.ini".content = ''
        [admins]
        ${config.services.couchdb.adminUser} = ${serviceCfg.secrets.placeholder.admin-password}
      '';
      systemd.dependentServices = [ "couchdb" "couchdb-configure" ];
    };
  };

  services.couchdb = {
    enable = true;
    port = serviceCfg.port;
    extraConfigFiles = [ serviceCfg.secrets.templates."admin.ini".path ];
    extraConfig = {
      couchdb.single_node = true;
      chttpd_auth_lockout.mode = "warn";
      chttpd = {
        require_valid_user = true;
        require_valid_user_except_for_up = true;
        max_http_request_size = 104857600; # 100MB. Sufficient for Obsidian note sync
        enable_cors = true;
      };
      chttpd_auth.require_valid_user = true;
      httpd = {
        "WWW-Authenticate" = "Basic realm=\"couchdb\"";
        enable_cors = true;
      };
      couchdb.max_document_size = 50000000;
      cors = {
        origins = toCommaSeparated cors.origins;
        credentials = cors.credentials;
        methods = toCommaSeparated cors.methods;
        headers = toCommaSeparated cors.headers;
      };
    };
  };

  systemd.services.couchdb.serviceConfig.RuntimeDirectory = "couchdb";

  sops.secrets = lib.mapAttrs' (_: u: lib.nameValuePair
    "couchdb/${u.username}/password"
    { group = config.services.couchdb.group; mode = "0440"; }
  ) enabledUsers;

  systemd.services.couchdb-configure = {
    description = "CouchDB setup";
    wantedBy = [ "couchdb.service" ];
    after = [ "couchdb.service" ];
    requires = [ "couchdb.service" ];
    partOf = [ "couchdb.service" ];
    restartTriggers = [ ./couchdb-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      User = config.services.couchdb.user;
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

  # Obsidian LiveSync specific CouchDB settings
  # CORS middleware for Traefik (required for Obsidian Live Sync app clients)
  custom.homelab.services.couchdb.traefik.middlewares.couchdb-cors.headers = {
    accessControlAllowMethods = cors.methods;
    accessControlAllowHeaders = cors.headers;
    accessControlAllowOriginList = cors.origins;
    accessControlMaxAge = cors.maxAge;
    accessControlAllowCredentials = cors.credentials;
    addVaryHeader = true;
  };
}
