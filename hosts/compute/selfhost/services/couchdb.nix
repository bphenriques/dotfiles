{ config, lib, ... }:
let
  vault = import ./livesync-cli/vault.nix;

  # Obsidian's clients are the only reason CouchDB needs CORS at all, so the origins and the sizing
  # live here rather than in the framework app.
  cors = {
    origins = [
      "app://obsidian.md"
      "capacitor://localhost"
      "http://localhost"
    ];
    methods = [ "GET" "PUT" "POST" "HEAD" "DELETE" ];
    headers = [ "accept" "authorization" "content-type" "origin" "referer" ];
    credentials = true;
    maxAge = 3600;
  };
in
{
  selfhost = {
    apps.couchdb.enable = true;

    # The Obsidian vault database. Kept out of dotfiles-private, which holds identity rather than
    # per-app deployment detail.
    users.${vault.owner}.services.couchdb = {
      enable = true;
      databases = [ vault.database ];
    };

    services.couchdb.traefik.middlewares.couchdb-cors.headers = {
      accessControlAllowMethods = cors.methods;
      accessControlAllowHeaders = cors.headers;
      accessControlAllowOriginList = cors.origins;
      accessControlMaxAge = cors.maxAge;
      accessControlAllowCredentials = cors.credentials;
      addVaryHeader = true;
    };
  };

  services.couchdb.extraConfig = {
    chttpd = {
      enable_cors = true;
      max_http_request_size = 104857600; # 100MB, headroom over the 50MB document cap below
    };
    httpd.enable_cors = true;
    couchdb.max_document_size = 50000000; # an Obsidian attachment, not a note
    cors = {
      origins = lib.concatStringsSep ", " cors.origins;
      methods = lib.concatStringsSep ", " cors.methods;
      headers = lib.concatStringsSep ", " cors.headers;
      inherit (cors) credentials;
    };
  };

  # Outcome of troubleshooting ~2% idle CPU with no traffic: two schedulers, no busy-wait.
  systemd.services.couchdb = {
    environment.ERL_ZFLAGS = "+S 2:2 +SDcpu 2:2 +sbwt none +sbwtdcpu none +sbwtdio none";
    environment.ERL_EPMD_ADDRESS = "127.0.0.1";   # the port mapper daemon binds 0.0.0.0:4369 by default
    serviceConfig.RuntimeDirectory = "couchdb";
  };
}
