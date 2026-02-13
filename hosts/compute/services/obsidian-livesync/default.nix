{ config, lib, ... }:
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
    chttpd_auth_lockout.mode = "warn";
    chttpd = {
      require_valid_user = true;
      require_valid_user_except_for_up = true;
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
in
{
  imports = [ ./post-start.nix ];

  custom.home-server.services.obsidian-livesync = {
    port = 5984;
    dashboard = {
      enable = true;
      category = "Productivity";
      description = "Obsidian Sync";
      icon = "obsidian.svg";
    };
  };

  sops = {
    secrets."obsidian-livesync/admin/password" = { };
    templates = {
      "couchdb-admin-password" = {
        owner = config.services.couchdb.user;
        content = config.sops.placeholder."obsidian-livesync/admin/password";
      };
      "couchdb-admin" = {
        owner = config.services.couchdb.user;
        content = ''
          [admins]
          ${config.services.couchdb.adminUser} = ${config.sops.placeholder."obsidian-livesync/admin/password"}
        '';
      };
    };
  };

  services.couchdb = {
    enable = true;
    port = serviceCfg.port;
    extraConfig = couchdbSettings;
    extraConfigFiles = [ config.sops.templates."couchdb-admin".path ];
  };

  # Add CORS middleware for Traefik (required for Obsidian Live Sync)
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
