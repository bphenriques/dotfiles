{ lib, config, ... }:
let
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
  # Obsidian LiveSync specific CouchDB settings
  services.couchdb.extraConfig = {
    couchdb.max_document_size = 50000000;
    chttpd = {
      max_http_request_size = 4294967296;
      enable_cors = true;
    };
    httpd.enable_cors = true;
    cors = {
      origins = toCommaSeparated cors.origins;
      credentials = cors.credentials;
      methods = toCommaSeparated cors.methods;
      headers = toCommaSeparated cors.headers;
    };
  };

  # Add CORS middleware for Traefik (required for Obsidian Live Sync)
  services.traefik.dynamicConfigOptions.http = {
    middlewares.couchdb-cors.headers = {
      accessControlAllowMethods = cors.methods;
      accessControlAllowHeaders = cors.headers;
      accessControlAllowOriginList = cors.origins;
      accessControlMaxAge = cors.maxAge;
      accessControlAllowCredentials = cors.credentials;
      addVaryHeader = true;
    };

    routers.couchdb.middlewares = [ "couchdb-cors" ];
  };
}
