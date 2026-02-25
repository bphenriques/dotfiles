{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
  oidcClient = config.custom.homelab.oidc.clients.miniflux;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.miniflux = {
    port = 8081;
    dashboard = {
      enable = true;
      category = "Media";
      description = "RSS Server";
      icon = "miniflux.svg";
    };
  };

  custom.homelab.oidc.clients.miniflux = {
    systemd.dependentServices = [ "miniflux" "miniflux-configure" ];
  };

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
      OAUTH2_OIDC_PROVIDER_NAME = oidcCfg.provider.displayName;
      OAUTH2_CLIENT_ID_FILE = oidcClient.systemd.credentialPaths.id;
      OAUTH2_CLIENT_SECRET_FILE = oidcClient.systemd.credentialPaths.secret;
    };
  };

  systemd.services.miniflux.serviceConfig.LoadCredential = oidcClient.systemd.loadCredentials;
}
