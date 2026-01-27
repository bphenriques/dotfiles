{ config, ... }:
let
  serviceCfg = config.custom.home-server.routes.miniflux;
  oidcCfg = config.custom.home-server.oidc;
in
{
  custom.home-server.routes.miniflux.port = 8081;
  custom.home-server.oidc.clients.miniflux.systemd.enable = true;

  services.miniflux = {
    enable = true;

    createDatabaseLocally = true;
    adminCredentialsFile = config.sops.templates."miniflux-secrets".path;
    config = {
      LISTEN_ADDR = "127.0.0.1:${toString serviceCfg.port}";
      BASE_URL = serviceCfg.publicUrl;
      OAUTH2_PROVIDER = "oidc";
      OAUTH2_REDIRECT_URL = builtins.head oidcCfg.clients.miniflux.callbackURLs;
      OAUTH2_OIDC_DISCOVERY_ENDPOINT = oidcCfg.discoveryEndpoint;
      OAUTH2_OIDC_PROVIDER_NAME = "PocketId";
      RUN_MIGRATIONS = true;
      CREATE_ADMIN = true;

      OAUTH2_USER_CREATION = 1;
      DISABLE_LOCAL_AUTH = 0;
    };
  };

  sops = {
    secrets.miniflux_admin_username = { };
    secrets.miniflux_admin_password = { };
    secrets."pocket-id/oidc-clients/miniflux" = { };

    templates."miniflux-secrets" = {
      content = ''
        ADMIN_USERNAME=${config.sops.placeholder.miniflux_admin_username}
        ADMIN_PASSWORD=${config.sops.placeholder.miniflux_admin_password}
      '';
    };
  };
}