{ config, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./configure.nix ./backup.nix ];

  options.custom.homelab.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.miniflux = {
        enable = lib.mkEnableOption "Miniflux settings for this user";
        settings = lib.mkOption {
          type = lib.types.attrs;
          default = { };
          description = ''
            Miniflux user settings applied via API (theme, display_mode, stylesheet, etc).
            See https://miniflux.app/docs/api.html#update-user for available fields.
          '';
          example = { theme = "dark_serif"; display_mode = "fullscreen"; };
        };
      };
    });
  };

  config = {
    custom.homelab.services.miniflux = {
      displayName = "Miniflux";
      metadata.description = "RSS Server";
      metadata.version = config.services.miniflux.package.version;
      metadata.homepage = config.services.miniflux.package.meta.homepage;
      metadata.category = "General";
      port = 8081;
      access.allowedGroups = with config.custom.homelab.groups; [ admin ];
      oidc = {
        enable = true;
        systemd.dependentServices = [ "miniflux" "miniflux-configure" ];
      };
      healthcheck.path = "/healthcheck";
      integrations.homepage.enable = true;
    };

    services.miniflux = {
      enable = true;
      createDatabaseLocally = true;
      config = {
        LISTEN_ADDR = "127.0.0.1:${toString serviceCfg.port}";
        BASE_URL = serviceCfg.publicUrl;
        RUN_MIGRATIONS = true;

        # OAuth2
        OAUTH2_USER_CREATION = 1;
        OAUTH2_PROVIDER = "oidc";
        OAUTH2_REDIRECT_URL = builtins.head serviceCfg.oidc.callbackURLs;
        OAUTH2_OIDC_DISCOVERY_ENDPOINT = oidcCfg.provider.issuerUrl;
        OAUTH2_OIDC_PROVIDER_NAME = oidcCfg.provider.displayName;
        OAUTH2_CLIENT_ID_FILE = serviceCfg.oidc.id.file;
        OAUTH2_CLIENT_SECRET_FILE = serviceCfg.oidc.secret.file;
      };
    };

    systemd.services.miniflux.serviceConfig.SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;

    # Fix start-limit issue - dbsetup needs to stay "active" after completion
    systemd.services.miniflux-dbsetup.serviceConfig.RemainAfterExit = true;
  };
}
