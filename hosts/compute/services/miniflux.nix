{ config, ... }:
let
  port = 8081;
  publicUrl = config.custom.home-server.services.miniflux.publicUrl;
  oidcProvider = {
    name = "PocketId";
    discoveryEndpoint = config.custom.home-server.services.pocket-id.publicUrl;
  };
in
{
  custom.home-server.services.miniflux.internalUrl = "http://127.0.0.1:${toString port}";

  services.miniflux = {
    enable = true;

    createDatabaseLocally = true; # Automatic set up a postgres database.
    adminCredentialsFile = config.sops.templates."miniflux-secrets".path;
    config = {
      LISTEN_ADDR = "127.0.0.1:${toString port}";
      BASE_URL = publicUrl;
      OAUTH2_PROVIDER = "oidc";
      OAUTH2_REDIRECT_URL = "${publicUrl}/oauth2/oidc/callback";
      OAUTH2_OIDC_DISCOVERY_ENDPOINT = oidcProvider.discoveryEndpoint;
      OAUTH2_OIDC_PROVIDER_NAME = oidcProvider.name;
      RUN_MIGRATIONS = true;
      CREATE_ADMIN = true;

      # Not standard config, therefore setting up using signed ints.
      OAUTH2_USER_CREATION = 1;
      DISABLE_LOCAL_AUTH = 0; # Enabled explicitly as it is required to use mobile apps.
    };
  };

  sops = {
    secrets.miniflux_admin_username = { };
    secrets.miniflux_admin_password = { };
    secrets.miniflux_oidc_client_id = { };
    secrets.miniflux_oidc_client_secret = { };

    templates."miniflux-secrets" = {
      content = ''
        ADMIN_USERNAME=${config.sops.placeholder.miniflux_admin_username}
        ADMIN_PASSWORD=${config.sops.placeholder.miniflux_admin_password}
        OAUTH2_CLIENT_ID=${config.sops.placeholder.miniflux_oidc_client_id}
        OAUTH2_CLIENT_SECRET=${config.sops.placeholder.miniflux_oidc_client_secret}
      '';
    };
  };

}