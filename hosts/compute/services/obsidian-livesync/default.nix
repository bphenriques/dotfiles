{ lib, config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.couchdb;

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
  imports = [ ./configure.nix ];

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
  systemd.services.couchdb = {
    # Tune Erlang BEAM VM for low-traffic, single-user Obsidian sync.
    # Outcome of troubleshooting high idle CPU (~2%) with no active traffic.
    #
    # Scheduler count (defaults to 1 per CPU core):
    # - +S 2:2:       Use 2 normal schedulers (enough for HTTP + compaction concurrency)
    # - +SDcpu 2:2:   Use 2 dirty CPU schedulers (matches normal scheduler count)
    #
    # Busy-wait (defaults to spinning to reduce latency):
    # - +sbwt none:      Disable scheduler busy-wait threshold
    # - +sbwtdcpu none:  Disable dirty CPU scheduler busy-wait
    # - +sbwtdio none:   Disable dirty I/O scheduler busy-wait
    environment.ERL_ZFLAGS = "+S 2:2 +SDcpu 2:2 +sbwt none +sbwtdcpu none +sbwtdio none";
    serviceConfig.RuntimeDirectory = "couchdb";
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
