{ config, ... }:
let
  serviceCfg = config.custom.home-server.routes.immich;
  oidcCfg = config.custom.home-server.oidc;
  oidcClient = oidcCfg.clients.immich;
in
{
  imports = [ ./setup.nix ];

  custom.home-server = {
    routes.immich.port = 2283;
    oidc.clients.immich = {
      callbackURLs = [
        "${serviceCfg.publicUrl}/auth/login"
        "${serviceCfg.publicUrl}/user-settings"
        "app.immich:///oauth-callback"
      ];
    };
  };

  services.immich = {
    enable = true;
    host = serviceCfg.internalHost;
    port = serviceCfg.port;
    mediaLocation = "/var/lib/immich";
    accelerationDevices = null; # Give access to all devices

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
      passwordLogin.enabled = true; # Only for administrator
      library.watch.enabled = true; # Auto-import changes from external libraries

      storageTemplate = {
        enabled = true;
        hashVerificationEnabled = true;
        template = "{{y}}/{{y}}-{{MM}}-{{dd}}/{{filename}}";
      };
    };
  };
}
