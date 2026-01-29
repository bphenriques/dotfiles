{ config, ... }:
let
  serviceCfg = config.custom.home-server.routes.miniflux;
  oidcClient = config.custom.home-server.oidc.clients.miniflux;
  oidcCfg = config.custom.home-server.oidc;
in
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.miniflux.port = 8081;
  custom.home-server.oidc.clients.miniflux = { };

  services.miniflux = {
    enable = true;
    createDatabaseLocally = true;
    config = {
      LISTEN_ADDR = "127.0.0.1:${toString serviceCfg.port}";
      BASE_URL = serviceCfg.publicUrl;
      RUN_MIGRATIONS = true;

      # OAuth2
      OAUTH2_USER_CREATION = 1;
      OAUTH2_PROVIDER = "oidc";
      OAUTH2_REDIRECT_URL = builtins.head oidcClient.callbackURLs;
      OAUTH2_OIDC_DISCOVERY_ENDPOINT = oidcCfg.provider.url;
      OAUTH2_OIDC_PROVIDER_NAME = oidcCfg.provider.name;
      OAUTH2_CLIENT_ID_FILE = "/run/credentials/miniflux.service/oidc-id";
      OAUTH2_CLIENT_SECRET_FILE = "/run/credentials/miniflux.service/oidc-secret";
    };
  };

  # Wait for OIDC credentials; LoadCredential reads files as root before dropping privileges
  systemd.services.miniflux = {
    wants = [ oidcCfg.systemd.readyUnit ];
    after = [ oidcCfg.systemd.readyUnit oidcCfg.systemd.provisionUnit ];
    serviceConfig.LoadCredential = [
      "oidc-id:${oidcClient.idFile}"
      "oidc-secret:${oidcClient.secretFile}"
    ];
  };
}
