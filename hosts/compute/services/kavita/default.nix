{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.smb.mounts;

  kavitaCfg = config.services.kavita;
in
{
  imports = [ ./configure.nix ];

  options.custom.homelab.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.kavita = {
        enable = lib.mkEnableOption "Kavita permissions for this user";
        passwordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to file containing Kavita password for local authentication";
        };
      };
    });
  };

  config = {
    custom.homelab = {
      services.kavita = {
        displayName = "Kavita";
        description = "Book Server";
        category = "Media";
        port = 8097;
        access.allowedGroups = with config.custom.homelab.groups; [ guests users admin ];
        oidc = {
          enable = true;
          callbackURLs = [
            "${serviceCfg.publicUrl}/signin-oidc"
            "${serviceCfg.publicUrl}/signout-callback-oidc"
          ];
          systemd.dependentServices = [ "kavita" ];
        };
        healthcheck.path = "/api/health";
        integrations.homepage.enable = true;
        storage.smb = [ "media" ];
      };

      runtimeSecrets = {
        kavita-token-key = {
          bytes = 64;
          owner = "kavita";
          restartUnits = [ "kavita.service" ];
        };
        kavita-admin-password = {
          regenerateIfMissing = false;
          owner = "kavita";
          restartUnits = [ "kavita-configure.service" ];
        };
      };
    };

    services.kavita = {
      enable = true;
      tokenKeyFile = config.custom.homelab.runtimeSecrets.kavita-token-key.path;
      settings.Port = serviceCfg.port;
      settings.IpAddresses = "127.0.0.1";
      settings.OpenIdConnectSettings = {
        Authority = oidcCfg.provider.issuerUrl;
        ClientId = serviceCfg.oidc.id.placeholder;
        Secret = serviceCfg.oidc.secret.placeholder;
      };
    };

    systemd.services.kavita = {
      serviceConfig = {
        LoadCredential = serviceCfg.oidc.systemd.loadCredentials;
        ReadOnlyPaths = [
          pathsCfg.media.books.library
          pathsCfg.media.comics.library
          pathsCfg.media.manga.library
        ];
      };
      # Kavita has no native _FILE support for OIDC credentials; replace placeholders at runtime.
      # If upstream adds file-based config, switch to it and remove this workaround.
      preStart = lib.mkAfter ''
        ${pkgs.replace-secret}/bin/replace-secret '${serviceCfg.oidc.id.placeholder}' \
          "''${CREDENTIALS_DIRECTORY}/oidc-id" \
          '${kavitaCfg.dataDir}/config/appsettings.json'
        ${pkgs.replace-secret}/bin/replace-secret '${serviceCfg.oidc.secret.placeholder}' \
          "''${CREDENTIALS_DIRECTORY}/oidc-secret" \
          '${kavitaCfg.dataDir}/config/appsettings.json'
      '';
    };

    users.users.kavita.extraGroups = [ homelabMounts.media.group ];
  };
}
