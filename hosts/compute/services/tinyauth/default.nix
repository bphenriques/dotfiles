{ config, pkgs, lib, ... }:
let
  cfg = config.custom.home-server;
  oidcCfg = cfg.oidc;
  oidcClient = cfg.oidc.clients.tinyauth;
  serviceCfg = cfg.routes.tinyauth;
  dataDir = "/var/lib/tinyauth";
in
{
  custom.home-server.routes.tinyauth.port = 3000;
  custom.home-server.auth = {
    enable = true;
    port = serviceCfg.port;
  };

  custom.home-server.oidc.clients.tinyauth = {
    callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/callback/pocketid" ];
  };

  sops.secrets."tinyauth/secret" = { };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0755 root root -"
  ];

  virtualisation.oci-containers = {
    backend = "podman";
    containers.tinyauth = {
      image = "ghcr.io/steveiliop56/tinyauth:v4";
      autoStart = true;
      ports = [ "${serviceCfg.internalHost}:3000" ];
      environment = {
        APP_URL = serviceCfg.publicUrl;
        DISABLE_ANALYTICS = "true";
        SECURE_COOKIE = "true";
        LOG_LEVEL = "info";

        # Use OAuth auto-redirect to Pocket-ID (skip local login screen)
        OAUTH_AUTO_REDIRECT = "pocketid";

        # Pocket-ID OIDC configuration
        PROVIDERS_POCKETID_NAME = oidcCfg.provider.displayName;
        PROVIDERS_POCKETID_AUTH_URL = "${oidcCfg.provider.url}/authorize";
        PROVIDERS_POCKETID_TOKEN_URL = "${oidcCfg.provider.url}/api/oidc/token";
        PROVIDERS_POCKETID_USER_INFO_URL = "${oidcCfg.provider.url}/api/oidc/userinfo";
        PROVIDERS_POCKETID_REDIRECT_URL = "${serviceCfg.publicUrl}/api/oauth/callback/pocketid";
        PROVIDERS_POCKETID_SCOPES = "openid,profile,email";
        PROVIDERS_POCKETID_CLIENT_ID_FILE = "/run/secrets/client_id";
        PROVIDERS_POCKETID_CLIENT_SECRET_FILE = "/run/secrets/client_secret";
        SECRET_FILE = "/run/secrets/secret";
      };
      volumes = [
        "${dataDir}:/data"
        "${oidcClient.idFile}:/run/secrets/client_id:ro"
        "${oidcClient.secretFile}:/run/secrets/client_secret:ro"
        "${config.sops.secrets."tinyauth/secret".path}:/run/secrets/secret:ro"
      ];
      extraOptions = [
        "--read-only"
        "--cap-drop=ALL"
      ];
    };
  };

  # Ensure tinyauth waits for OIDC credentials to be provisioned
  systemd.services.podman-tinyauth = {
    after = [ oidcCfg.systemd.provisionUnit ];
    wants = [ oidcCfg.systemd.readyUnit ];
    serviceConfig.SupplementaryGroups = [ oidcClient.group ];
  };
}
