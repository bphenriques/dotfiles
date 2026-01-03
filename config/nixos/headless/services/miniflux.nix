_: {
  services.miniflux = {
    enable = true;

    createDatabaseLocally = true;
    adminCredentialsFile = "";# ADMIN_USERNAME=... ADMIN_PASSWORD=...
    config = {
      RUN_MIGRATIONS = true;
      CREATE_ADMIN = true;
      BASE_URL = "https://miniflux.${HOME_SERVER_BASE_URL}";
      OAUTH2_PROVIDER = "oidc";
      OAUTH2_REDIRECT_URL = "https://miniflux.${HOME_SERVER_BASE_URL}/oauth2/oidc/callback";
      OAUTH2_OIDC_DISCOVERY_ENDPOINT = "https://pocket-id.${HOME_SERVER_BASE_URL}";
      OAUTH2_OIDC_PROVIDER_NAME = "PocketID";
      OAUTH2_USER_CREATION = true;
      DISABLE_LOCAL_AUTH = false; # Keep it enabled explicitly. This way I can use mobile apps



      #ADMIN_USERNAME=test
      #ADMIN_PASSWORD=test123456

      #OAUTH2_CLIENT_ID=<client ID>
      #OAUTH2_CLIENT_SECRET=<client secret>
    };
  };
}