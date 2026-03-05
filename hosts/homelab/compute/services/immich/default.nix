{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.immich;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.immich = {
    port = 2283;
    subdomain = "photos";
    integrations.homepage = {
      enable = true;
      category = "Media";
      description = "Photo & Video Gallery";
    };

    # TODO: admin-password could be removed if Immich supports OIDC-only admin
    secrets = {
      files.admin-password = { rotatable = false; };
      systemd.dependentServices = [ "immich-configure" ];
    };

    oidc = {
      enable = true;
      callbackURLs = [
        "${serviceCfg.publicUrl}/auth/login"
        "${serviceCfg.publicUrl}/user-settings"
        "app.immich:///oauth-callback"
      ];
      systemd.dependentServices = [ "immich-server" ];
    };
  };

  services.immich = {
    enable = true;
    host = serviceCfg.host;
    port = serviceCfg.port;
    mediaLocation = "/var/lib/immich";
    accelerationDevices = null;

    settings = {
      server.externalDomain = serviceCfg.publicUrl;
      newVersionCheck.enabled = false;

      oauth = {
        enabled = true;
        issuerUrl = oidcCfg.provider.url;
        clientId._secret = serviceCfg.oidc.idFile;
        clientSecret._secret = serviceCfg.oidc.secretFile;
        scope = "openid email profile";
        signingAlgorithm = "RS256";
        buttonText = "Login with ${oidcCfg.provider.displayName}";
        autoRegister = true;
        autoLaunch = false;
      };
      passwordLogin.enabled = true;
      library.watch.enabled = true;

      storageTemplate = {
        enabled = true;
        hashVerificationEnabled = true;
        template = "{{y}}/{{y}}-{{MM}}-{{dd}}/{{filename}}";
      };
    };
  };

  systemd.services.immich-server.serviceConfig.SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups ++ [ "video" "render" ];
}
