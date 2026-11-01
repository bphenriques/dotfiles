{ config, lib, ... }:
let
  serviceCfg = config.selfhost.services.kavita;
  oidcCfg = config.selfhost.auth.oidc;
  sharesCfg = config.fleet.shares;
in
{
  imports = [ ./configure.nix ];

  options.selfhost.users = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { config, ... }:
        {
          options.services.kavita = {
            enable = lib.mkEnableOption "Kavita permissions for this user";
            passwordFile = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "Path to file containing Kavita password for local authentication";
            };
            admin = lib.mkOption {
              type = lib.types.bool;
              default = config.isAdmin; # config = the user submodule (identity), reached via closure
              description = "Grant the Kavita Admin role; defaults to the user's fleet isAdmin.";
            };
          };
        }
      )
    );
  };

  config = {
    selfhost = {
      services.kavita = {
        displayName = "Kavita";
        meta.homepage = "https://www.kavitareader.com";
        meta.description = "Book Server";
        meta.category = "media";
        port = 8097;
        access.allowedGroups = with config.selfhost.groups; [ guests users admin ];
        access.model = "oidc";
        access.oidc = {
          callbackURLs = [
            "${serviceCfg.publicUrl}/signin-oidc"
            "${serviceCfg.publicUrl}/signout-callback-oidc"
          ];
          systemd.dependentServices = [ "kavita" ];
        };
        healthcheck.path = "/api/health";
        storage.mounts = [ "media" ];
        storage.users = [ "kavita" ];
        extraConfig.landingPage.enable = true;
      };

      runtimeSecrets = {
        kavita-token-key = {
          bytes = 64;
          owner = "kavita";
          restartUnits = [ "kavita.service" ];
        };
        kavita-admin-password = {
          onMissing = "warn";
          owner = "kavita";
          restartUnits = [ "kavita-configure.service" ];
        };
      };
    };

    services.kavita = {
      enable = true;
      credentials = {
        token_key = config.selfhost.runtimeSecrets.kavita-token-key.path;
        oidc_id = serviceCfg.access.oidc.id.file;
        oidc_secret = serviceCfg.access.oidc.secret.file;
      };
      settings.Port = serviceCfg.port;
      settings.IpAddresses = "127.0.0.1";
      settings.OpenIdConnectSettings = {
        Authority = oidcCfg.provider.issuerUrl;
        ClientId = "@oidc_id@";
        Secret = "@oidc_secret@";
      };
    };

    systemd.services.kavita.serviceConfig.ReadOnlyPaths = [
      "${sharesCfg.media.root}/books/library"
      "${sharesCfg.media.root}/comics/library"
      "${sharesCfg.media.root}/manga/library"
    ];
  };
}
