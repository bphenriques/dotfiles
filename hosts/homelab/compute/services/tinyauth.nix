{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.homelab;
  oidcCfg = cfg.oidc;
  serviceCfg = cfg.services.tinyauth;

  dataDir = "/var/lib/tinyauth";
  envFile = "/run/tinyauth/env";
in
{
  custom.homelab = {
    services.tinyauth = {
      port = 3000;
      secrets = {
        files.secret = { rotatable = true; };
        systemd.dependentServices = [ "podman-tinyauth" ];
      };
      oidc = {
        enable = true;
        callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/callback/pocketid" ];
        systemd.dependentServices = [ "podman-tinyauth" ];
      };
      integrations.homepage = {
        enable = true;
        category = "Admin";
        description = "Auth Proxy";
      };
    };

    ingress.forwardAuth = {
      enable = true;
      url = serviceCfg.url;
    };
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0750 root root -"
  ];

  virtualisation.oci-containers.containers.tinyauth = {
    image = "ghcr.io/steveiliop56/tinyauth:v4.0.1";
    autoStart = true;
    ports = [ "${serviceCfg.host}:${toString serviceCfg.port}:3000" ];
    environment = {
      APP_URL = serviceCfg.publicUrl;
      DISABLE_ANALYTICS = "true";
      SECURE_COOKIE = "true";
      LOG_LEVEL = "info";
      OAUTH_AUTO_REDIRECT = "pocketid";

      # Pocket-ID OIDC configuration
      PROVIDERS_POCKETID_NAME = oidcCfg.provider.displayName;
      PROVIDERS_POCKETID_AUTH_URL = "${oidcCfg.provider.url}/authorize";
      PROVIDERS_POCKETID_TOKEN_URL = "${oidcCfg.provider.url}/api/oidc/token";
      PROVIDERS_POCKETID_USER_INFO_URL = "${oidcCfg.provider.url}/api/oidc/userinfo";
      PROVIDERS_POCKETID_REDIRECT_URL = "${serviceCfg.publicUrl}/api/oauth/callback/pocketid";
      PROVIDERS_POCKETID_SCOPES = "openid,profile,email,groups";
      PROVIDERS_POCKETID_CLIENT_SECRET_FILE = "/run/secrets/client_secret";
      SECRET_FILE = "/run/secrets/secret";
    }
    // lib.listToAttrs (
      lib.mapAttrsToList (name: svc: {
        name = "TINYAUTH_APPS_${lib.toUpper name}_OAUTH_GROUPS";
        value = svc.forwardAuth.group;
      }) (lib.filterAttrs (_: s: s.forwardAuth.enable) cfg.services)
    );

    environmentFiles = [ envFile ];
    volumes = [
      "${dataDir}:/data"
      "${serviceCfg.oidc.secretFile}:/run/secrets/client_secret:ro"
      "${serviceCfg.secrets.files.secret.path}:/run/secrets/secret:ro"
    ];
    extraOptions = [
      "--group-add=${toString (config.users.groups.${serviceCfg.oidc.group}.gid)}"
      "--memory=256m"
    ];
  };

  systemd.services.podman-tinyauth = {
    serviceConfig = {
      RuntimeDirectory = "tinyauth";
      SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
    };
    preStart = ''
      # Generate OIDC client ID env file
      echo "PROVIDERS_POCKETID_CLIENT_ID=$(cat ${serviceCfg.oidc.idFile})" > "${envFile}"
      chmod 600 "${envFile}"
    '';
  };
}
