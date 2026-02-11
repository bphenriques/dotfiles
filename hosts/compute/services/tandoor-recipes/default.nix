{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.home-server.routes.tandoor;
  oidcCfg = config.custom.home-server.oidc;
  oidcClient = oidcCfg.clients.tandoor;

  oidcConfigDir = "/var/lib/tandoor-recipes/oidc";
  oidcConfigFile = "${oidcConfigDir}/socialaccount-providers.json";

  oidcConfigTemplate = builtins.toJSON {
    openid_connect = {
      OAUTH_PKCE_ENABLED = false;
      APPS = [{
        provider_id = "pocketid";
        name = oidcCfg.provider.displayName;
        client_id = "__OIDC_CLIENT_ID__";
        secret = "__OIDC_CLIENT_SECRET__";
        settings.server_url = oidcCfg.provider.url;
      }];
    };
  };
in
{
  imports = [ ./setup.nix ];

  # TODO: look into norish and kitchenowl
  custom.home-server.routes.tandoor = {
    subdomain = "recipes";
    port = 9092;
  };
  custom.home-server.oidc.clients.tandoor = { };

  services.tandoor-recipes = {
    enable = true;
    port = serviceCfg.port;
    address = serviceCfg.internalHost;

    extraConfig = {
      TIMEZONE = config.time.timeZone;
      ALLOWED_HOSTS = serviceCfg.publicHost;
      CSRF_TRUSTED_ORIGINS = serviceCfg.publicUrl;
      MEDIA_ROOT = "/var/lib/tandoor-recipes/media";

      # OIDC via django-allauth
      SOCIAL_PROVIDERS = "allauth.socialaccount.providers.openid_connect";
      SOCIALACCOUNT_PROVIDERS_FILE = oidcConfigFile;

      # Auto-join OIDC users to the default space with "user" role
      SOCIAL_DEFAULT_ACCESS = 1;
      SOCIAL_DEFAULT_GROUP = config.custom.home-server.groups.users;
    };
  };

  systemd.tmpfiles.rules = [
    "d ${oidcConfigDir} 0750 tandoor_recipes ${oidcClient.group} -"
  ];

  systemd.services.tandoor-recipes = {
    wants = [ oidcCfg.systemd.provisionedTarget ];
    after = [ oidcCfg.systemd.provisionedTarget ];
    serviceConfig.SupplementaryGroups = [ oidcClient.group ];
  };

  # Generate OIDC config file with real credentials before tandoor starts
  systemd.services.tandoor-recipes-oidc-config = {
    description = "Generate Tandoor OIDC configuration";
    requiredBy = [ "tandoor-recipes.service" ];
    before = [ "tandoor-recipes.service" ];
    after = [ oidcCfg.systemd.provisionedTarget ];
    wants = [ oidcCfg.systemd.provisionedTarget ];
    restartTriggers = [ oidcConfigTemplate ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "tandoor_recipes";
      Group = "tandoor_recipes";
      LoadCredential = [
        "oidc-id:${oidcClient.idFile}"
        "oidc-secret:${oidcClient.secretFile}"
      ];
    };
    path = [ pkgs.nushell ];
    script = ''
      nu -c '
        let client_id = open $"($env.CREDENTIALS_DIRECTORY)/oidc-id" | str trim
        let client_secret = open $"($env.CREDENTIALS_DIRECTORY)/oidc-secret" | str trim

        open ${pkgs.writeText "oidc-template.json" oidcConfigTemplate}
        | update openid_connect.APPS.0.client_id $client_id
        | update openid_connect.APPS.0.secret $client_secret
        | save --force ${oidcConfigFile}

        chmod 640 ${oidcConfigFile}
      '
    '';
  };
}
