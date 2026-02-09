{ config, pkgs, lib, ... }:
let
  cfg = config.custom.home-server;
  oidcCfg = cfg.oidc;
  oidcClient = cfg.oidc.clients.tinyauth;
  serviceCfg = cfg.routes.tinyauth;

  dataDir = "/var/lib/tinyauth";
  envFile = "/run/tinyauth/env";
in
{
  custom.home-server.routes.tinyauth.port = 3000;
  custom.home-server.forwardAuth = {
    enable = true;
    internalUrl = serviceCfg.internalUrl;
  };

  custom.home-server.oidc.clients.tinyauth.callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/callback/pocketid" ];

  sops.secrets."tinyauth/secret" = { };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0755 root root -"
  ];

  virtualisation.oci-containers = {
    backend = "podman";
    containers.tinyauth = {
      image = "ghcr.io/steveiliop56/tinyauth:4.1.0";
      autoStart = true;
      ports = [  "${serviceCfg.internalHost}:${toString serviceCfg.port}:3000" ];
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
      } // lib.listToAttrs (lib.mapAttrsToList (name: route: {
        name = "TINYAUTH_APPS_${lib.toUpper name}_OAUTH_GROUPS";
        value = route.forwardAuth.group;
      }) (lib.filterAttrs (_: r: r.forwardAuth.enable) cfg.routes));  # FIXME: This is likely only available next version of tinyauth

      environmentFiles = [ envFile ];
      volumes = [
        "${dataDir}:/data"
        "${oidcClient.secretFile}:/run/secrets/client_secret:ro"
        "${config.sops.secrets."tinyauth/secret".path}:/run/secrets/secret:ro"
      ];
      extraOptions = [
        "--read-only"                            # immutable filesystem
        "--group-add=${toString (config.users.groups.${oidcClient.group}.gid)}" # read OIDC credentials
        "--tmpfs=/tmp:rw,noexec,nosuid,size=64m" # ephemeral tmp, no exec
        "--memory=256m"                          # OOM-kill if exceeded
      ];
    };
  };

  systemd.services.podman-tinyauth = {
    after = [ oidcCfg.systemd.provisionedTarget ];
    wants = [ oidcCfg.systemd.provisionedTarget ];
    serviceConfig.SupplementaryGroups = [ oidcClient.group ];
    # Generate env file for client_id (tinyauth doesn't support _FILE for this)
    serviceConfig.ExecStartPre = let
      script = pkgs.writeShellScript "tinyauth-env" ''
        mkdir -p "$(dirname "${envFile}")"
        echo "PROVIDERS_POCKETID_CLIENT_ID=$(cat ${oidcClient.idFile})" > "${envFile}"
        chmod 600 "${envFile}"
      '';
    in [ "!${script}" ];
  };
}
