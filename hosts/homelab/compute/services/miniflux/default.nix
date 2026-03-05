{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.miniflux = {
    port = 8081;
    oidc = {
      enable = true;
      systemd.dependentServices = [ "miniflux" "miniflux-configure" ];
    };
    # TODO: https://gethomepage.dev/widgets/services/miniflux/
    integrations.homepage = {
      enable = true;
      category = "Media";
      description = "RSS Server";
    };
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
      OAUTH2_REDIRECT_URL = builtins.head serviceCfg.oidc.callbackURLs;
      OAUTH2_OIDC_DISCOVERY_ENDPOINT = oidcCfg.provider.url;
      OAUTH2_OIDC_PROVIDER_NAME = oidcCfg.provider.displayName;
      OAUTH2_CLIENT_ID_FILE = serviceCfg.oidc.idFile;
      OAUTH2_CLIENT_SECRET_FILE = serviceCfg.oidc.secretFile;
    };
  };

  systemd.services.miniflux.serviceConfig.SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;

  # Fix start-limit issue - dbsetup needs to stay "active" after completion
  systemd.services.miniflux-dbsetup.serviceConfig.RemainAfterExit = true;
}
