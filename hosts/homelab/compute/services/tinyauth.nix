{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.homelab;
  oidcCfg = cfg.oidc;
  oidcClient = cfg.oidc.clients.tinyauth;
  serviceCfg = cfg.services.tinyauth;

  dataDir = "/var/lib/tinyauth";
  envFile = "/run/tinyauth/env";
in
{
  custom.homelab.services.tinyauth = {
    port = 3000;
    dashboard = {
      enable = false; # Internal service, not shown on dashboard
      category = "Infrastructure";
      description = "Auth Proxy";
      icon = "tinyauth.svg";
    };
  };

  custom.homelab.ingress.traefik.forwardAuth = {
    enable = true;
    internalUrl = serviceCfg.internalUrl;
  };

  custom.homelab.oidc.clients.tinyauth = {
    callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/callback/pocketid" ];
    systemd.dependentServices = [ "podman-tinyauth" ];
  };

  sops.secrets."tinyauth/secret" = { };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0750 root root -"
  ];

  virtualisation.oci-containers.containers.tinyauth = {
    image = "ghcr.io/steveiliop56/tinyauth:v4.0.1";
    autoStart = true;
    ports = [ "${serviceCfg.internalHost}:${toString serviceCfg.port}:3000" ];
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
      "${oidcClient.secretFile}:/run/secrets/client_secret:ro"
      "${config.sops.secrets."tinyauth/secret".path}:/run/secrets/secret:ro"
    ];
    extraOptions = [
      "--group-add=${toString (config.users.groups.${oidcClient.group}.gid)}"
      "--memory=256m"
    ];
  };

  systemd.services.podman-tinyauth = {
    serviceConfig.SupplementaryGroups = oidcClient.systemd.supplementaryGroups;
    preStart = ''
      mkdir -p "$(dirname "${envFile}")"
      echo "PROVIDERS_POCKETID_CLIENT_ID=$(cat ${oidcClient.idFile})" > "${envFile}"
      chmod 600 "${envFile}"
    '';
  };
}
