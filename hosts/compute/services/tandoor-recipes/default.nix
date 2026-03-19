{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.tandoor;
  oidcCfg = config.custom.homelab.oidc;
  pkg = config.services.tandoor-recipes.package;
in
{
  imports = [ ./setup.nix ./backup.nix ];

  custom.homelab.services.tandoor = {
    description = "Recipe Manager";
    version = pkg.version;
    homepage = pkg.meta.homepage;
    category = "General";
    port = 9092;
    subdomain = "recipes";
    aliases = [ "tandoor" ];
    secrets = {
      files.secret-key = { rotatable = true; bytes = 50; };
      templates.env.content = ''
        SECRET_KEY=${serviceCfg.secrets.placeholder.secret-key}
      '';
      templates."socialaccount-providers.json".content = builtins.toJSON {
        openid_connect = {
          OAUTH_PKCE_ENABLED = false;
          APPS = [{
            provider_id = oidcCfg.provider.internalName;
            name = oidcCfg.provider.displayName;
            client_id = serviceCfg.oidc.id.placeholder;
            secret = serviceCfg.oidc.secret.placeholder;
            settings.server_url = "${oidcCfg.provider.issuerUrl}/.well-known/openid-configuration";
          }];
        };
      };
      systemd.dependentServices = [ "tandoor-recipes" "tandoor-recipes-superuser" ];
    };
    oidc = {
      enable = true;
      allowedGroups = with config.custom.homelab.groups; [ users admin ];
      systemd.dependentServices = [ "tandoor-recipes" ];
    };
    integrations.homepage.enable = true;
    integrations.homepage.icon = "tandoor-recipes.svg";
    integrations.catalogue.displayName = "Tandoor";
    healthcheck.path = "/accounts/login/";
  };

  services.tandoor-recipes = {
    enable = true;
    port = serviceCfg.port;
    address = "127.0.0.1";
    database.createLocally = true;
    extraConfig = {
      TIMEZONE = config.time.timeZone;
      MEDIA_ROOT = "/var/lib/tandoor-recipes/media";
      ALLOWED_HOSTS = serviceCfg.publicHost;
      CSRF_TRUSTED_ORIGINS = serviceCfg.publicUrl;

      SOCIAL_PROVIDERS = "allauth.socialaccount.providers.openid_connect";
      SOCIALACCOUNT_PROVIDERS_FILE = serviceCfg.secrets.templates."socialaccount-providers.json".path;

      SOCIAL_DEFAULT_ACCESS = 1;
      SOCIAL_DEFAULT_GROUP = "user";
    };
  };

  systemd.services.tandoor-recipes = {
    serviceConfig.EnvironmentFile = [ serviceCfg.secrets.templates.env.path ];
    serviceConfig.SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
  };
}
