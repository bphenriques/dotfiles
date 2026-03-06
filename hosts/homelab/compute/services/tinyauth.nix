{ config, pkgs, lib, ... }:
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

  # TODO: Add health check when podman supports healthcheck restart policy
  virtualisation.oci-containers.containers.tinyauth = {
    image = "ghcr.io/steveiliop56/tinyauth:v5.0.1";
    autoStart = true;
    ports = [ "${serviceCfg.host}:${toString serviceCfg.port}:3000" ];
    environment = {
      TINYAUTH_APPURL = serviceCfg.publicUrl;
      TINYAUTH_ANALYTICS_ENABLED = "false";
      TINYAUTH_AUTH_SECURECOOKIE = "true";
      TINYAUTH_LOG_LEVEL = "info";

      # Pocket-ID OIDC configuration
      TINYAUTH_OAUTH_PROVIDERS_POCKETID_NAME = oidcCfg.provider.displayName;
      TINYAUTH_OAUTH_PROVIDERS_POCKETID_AUTHURL = "${oidcCfg.provider.url}/authorize";
      TINYAUTH_OAUTH_PROVIDERS_POCKETID_TOKENURL = "${oidcCfg.provider.url}/api/oidc/token";
      TINYAUTH_OAUTH_PROVIDERS_POCKETID_USERINFOURL = "${oidcCfg.provider.url}/api/oidc/userinfo";
      TINYAUTH_OAUTH_PROVIDERS_POCKETID_REDIRECTURL = "${serviceCfg.publicUrl}/api/oauth/callback/pocketid";
      TINYAUTH_OAUTH_PROVIDERS_POCKETID_SCOPES = "openid profile email groups";
      TINYAUTH_OAUTH_PROVIDERS_POCKETID_CLIENTSECRETFILE = "/run/secrets/client_secret";
    } // lib.listToAttrs (
      lib.mapAttrsToList (name: svc: {
        name = "TINYAUTH_APPS_${lib.toUpper name}_OAUTH_GROUPS"; # ACLs
        value = svc.forwardAuth.group;
      }) (lib.filterAttrs (_: s: s.forwardAuth.enable) cfg.services)
    );
    environmentFiles = [ envFile ];
    volumes = [
      "${dataDir}:/data"
      "${serviceCfg.oidc.secretFile}:/run/secrets/client_secret:ro"
    ];
    extraOptions = [
      "--group-add=${toString (config.users.groups.${serviceCfg.oidc.group}.gid)}"
      "--memory=256m"
    ];
  };

  # Unfortunately required as we can't pass the id as plain file (not that it is a secret)
  systemd.services.podman-tinyauth = {
    serviceConfig = {
      RuntimeDirectory = "tinyauth";
      LoadCredential = serviceCfg.oidc.systemd.loadCredentials;
    };
    preStart = ''
      echo "TINYAUTH_OAUTH_PROVIDERS_POCKETID_CLIENTID=$(cat $CREDENTIALS_DIRECTORY/oidc-id)" > "${envFile}"
      chmod 600 "${envFile}"
    '';
  };
}
