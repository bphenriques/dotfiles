{ config, lib, ... }:
let
  cfg = config.custom.homelab;
  oidcCfg = cfg.oidc;
  serviceCfg = cfg.services.tinyauth;
in
{
  custom.homelab = {
    services.tinyauth = {
      description = "ForwardAuth Gateway";
      category = "Infrastructure";
      version = config.services.tinyauth.package.version;
      homepage = config.services.tinyauth.package.meta.homepage;
      port = 3000;
      oidc = {
        enable = true;
        callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/callback/pocketid" ];
        systemd.dependentServices = [ "tinyauth" ];
      };
      secrets = {
        templates.env.content = ''
          TINYAUTH_OAUTH_PROVIDERS_POCKETID_CLIENTID=${serviceCfg.oidc.id.placeholder}
        '';
        systemd.dependentServices = [ "tinyauth" ];
      };
    };

    ingress.forwardAuth = {
      enable = true;
      url = serviceCfg.url;
    };
  };

  services.tinyauth = {
    enable = true;
    environmentFile = serviceCfg.secrets.templates.env.path;
    settings = {
      APPURL = serviceCfg.publicUrl;
      SERVER_ADDRESS = serviceCfg.host;
      SERVER_PORT = serviceCfg.port;
      ANALYTICS_ENABLED = false;
      AUTH_SECURECOOKIE = true;
      LOG_LEVEL = "info";

      # Pocket-ID OIDC configuration
      OAUTH_PROVIDERS_POCKETID_NAME = oidcCfg.provider.displayName;
      OAUTH_PROVIDERS_POCKETID_AUTHURL = "${oidcCfg.provider.issuerUrl}/authorize";
      OAUTH_PROVIDERS_POCKETID_TOKENURL = "${oidcCfg.provider.issuerUrl}/api/oidc/token";
      OAUTH_PROVIDERS_POCKETID_USERINFOURL = "${oidcCfg.provider.issuerUrl}/api/oidc/userinfo";
      OAUTH_PROVIDERS_POCKETID_REDIRECTURL = "${serviceCfg.publicUrl}/api/oauth/callback/pocketid";
      OAUTH_PROVIDERS_POCKETID_SCOPES = "openid profile email groups";
      OAUTH_PROVIDERS_POCKETID_CLIENTSECRETFILE = serviceCfg.oidc.secret.file;
    } // lib.listToAttrs (
      lib.mapAttrsToList (name: svc: {
        name = "APPS_${lib.toUpper name}_OAUTH_GROUPS";
        value = svc.forwardAuth.group;
      }) (lib.filterAttrs (_: s: s.forwardAuth.enable) cfg.services)
    );
  };

  systemd.services.tinyauth.serviceConfig.SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
}
