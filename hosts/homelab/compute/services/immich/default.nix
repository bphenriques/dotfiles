{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.immich;
  oidcCfg = config.custom.homelab.oidc;
  oidcClient = oidcCfg.clients.immich;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.immich = {
    port = 2283;
    subdomain = "photos";
    dashboard = {
      enable = true;
      category = "Media";
      description = "Photo & Video Gallery";
      icon = "immich.svg";
    };
    # TODO https://gethomepage.dev/widgets/services/immich/
  };

  custom.homelab.oidc.clients.immich = {
    callbackURLs = [
      "${serviceCfg.publicUrl}/auth/login"
      "${serviceCfg.publicUrl}/user-settings"
      "app.immich:///oauth-callback"
    ];
    systemd.dependentServices = [ "immich-server" ];
  };

  services.immich = {
    enable = true;
    host = serviceCfg.internalHost;
    port = serviceCfg.port;
    mediaLocation = "/var/lib/immich";
    accelerationDevices = null;

    settings = {
      server.externalDomain = serviceCfg.publicUrl;
      newVersionCheck.enabled = false;

      oauth = {
        enabled = true;
        issuerUrl = oidcCfg.provider.url;
        clientId._secret = oidcClient.idFile;
        clientSecret._secret = oidcClient.secretFile;
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

  systemd.services.immich-server.serviceConfig.SupplementaryGroups = oidcClient.systemd.supplementaryGroups;
}
