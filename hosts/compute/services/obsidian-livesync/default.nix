{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.home-server.services.obsidian-livesync;

  # Shared CORS configuration (used by both CouchDB and Traefik)
  cors = {
    origins = [ "app://obsidian.md" "capacitor://localhost" "http://localhost" ];
    methods = [ "GET" "PUT" "POST" "HEAD" "DELETE" ];
    headers = [ "accept" "authorization" "content-type" "origin" "referer" ];
    credentials = true;
    maxAge = 3600;
  };

  toCommaSeparated = lib.concatStringsSep ", ";
  couchdbSettings = {
    couchdb = {
      single_node = true;
      max_document_size = 50000000;
    };
    chttpd = {
      require_valid_user = true;
      max_http_request_size = 4294967296;
      enable_cors = true;
    };
    chttpd_auth.require_valid_user = true;
    httpd = {
      "WWW-Authenticate" = "Basic realm=\"couchdb\"";
      enable_cors = true;
    };
    cors = {
      origins = toCommaSeparated cors.origins;
      credentials = cors.credentials;
      methods = toCommaSeparated cors.methods;
      headers = toCommaSeparated cors.headers;
    };
  };

  # Filter users with obsidian-livesync enabled and format for init script
  enabledUsers = lib.filterAttrs (_: u: u.services.obsidian-livesync.enable) config.custom.home-server.users;
  users = lib.mapAttrsToList (_: u: {
    name = u.username;
    passwordFile = u.services.obsidian-livesync.passwordFile;
  }) enabledUsers;

  databases = lib.concatLists (lib.mapAttrsToList (_: u:
    map (db: { name = db; owner = u.username; }) u.services.obsidian-livesync.databases
  ) enabledUsers);
in
{
  custom.home-server.services.obsidian-livesync.port = 5984;
  services.couchdb = {
    enable = true;
    port = serviceCfg.port;
    extraConfig = lib.generators.toINI { } couchdbSettings;
    extraConfigFiles = [ config.sops.templates."couchdb-admin".path ];
  };

  sops = {
    secrets.obsidian_livesync_admin_password = { };
    templates = {
      "couchdb-admin-password" = {
        owner = config.services.couchdb.user;
        content = config.sops.placeholder.obsidian_livesync_admin_password;
      };
      "couchdb-admin" = {
        owner = config.services.couchdb.user;
        content = ''
          [admins]
          ${config.services.couchdb.adminUser} = ${config.sops.placeholder.obsidian_livesync_admin_password}
        '';
      };
    };
  };

  # Initialize databases and users after CouchDB starts
  systemd.services.couchdb-init = {
    description = "Initialize CouchDB databases and users";
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
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    environment = {
      COUCHDB_URL = "http://${config.services.couchdb.bindAddress}:${toString config.services.couchdb.port}";
      COUCHDB_ADMIN_USER = config.services.couchdb.adminUser;
      COUCHDB_ADMIN_PASS_FILE = config.sops.templates."couchdb-admin-password".path;
      COUCHDB_USERS_JSON = builtins.toJSON users;
      COUCHDB_DBS_JSON = builtins.toJSON databases;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${./couchdb-init.nu}'';
  };

  # Add CORS middleware for Traefik (required for Obsidian app)
  services.traefik.dynamicConfigOptions.http = {
    middlewares.obsidian-livesync-cors.headers = {
      accessControlAllowMethods = cors.methods;
      accessControlAllowHeaders = cors.headers;
      accessControlAllowOriginList = cors.origins;
      accessControlMaxAge = cors.maxAge;
      accessControlAllowCredentials = cors.credentials;
      addVaryHeader = true;
    };

    routers.obsidian-livesync.middlewares = [ "obsidian-livesync-cors" ];
  };
}
