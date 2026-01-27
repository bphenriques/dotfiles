{ config, pkgs, ... }:
let
  openId = {
    openid_connect.APPS = [
      {
        provider_id = "pocketid";
        name = "Pocket ID";
        client_id = "CLIENT_ID";
        secret = "CLIENT_SECRET";
        settings.server_url = "https://pocket-id.domain/.well-known/openid-configuration";
      }
    ];
  };
in
{
  custom.home-server.routes.tandoor.port = 9092;
  custom.home-server.routes.tandoor.subdomain = "recipes";

  services.tandoor-recipes = {
    enable = true;
    database.createLocally = true;
    port = config.custom.home-server.routes.tandoor.port;
    extraConfig = {
      ALLOWED_HOSTS = config.custom.home-server.routes.tandoor.host;
      SOCIAL_PROVIDERS = "allauth.socialaccount.providers.openid_connect";
      SOCIAL_DEFAULT_GROUP = "user";
 
      # FIXME
      # random secret key, use for example `base64 /dev/urandom | head -c50` to generate one
      #SECRET_KEY_FILE = "test";
      # SOCIALACCOUNT_PROVIDERS = toJson openId;
    };
  };
}