{ config, lib, ... }:
let
  serviceCfg = config.selfhost.services.seerr;
in
{
  imports = [ ./configure.nix ];

  options.custom.users = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options.services.seerr = {
          enable = lib.mkEnableOption "Seerr account for this user (requires Jellyfin)";
          permissions = {
            autoApprove = lib.mkEnableOption "auto-approve requests";
            advancedRequests = lib.mkEnableOption "advanced request options (e.g., quality profile)";
            viewRecentlyAdded = lib.mkEnableOption "view recently added media";
          };
        };
      }
    );
  };

  # Auth is delegated to Jellyfin (users sign in via Jellyfin credentials, not direct OIDC/forwardAuth)
  config = {
    selfhost = {
      services.seerr = {
        displayName = "Seerr";
        description = "TV / Movie Finder";
        port = 9099;
        healthcheck.path = "/api/v1/status";
      };

      # Upstream uses DynamicUser; EnvironmentFile loads as root before user drop.
      runtimeSecrets.seerr-api-key = {
        restartUnits = [ "seerr.service" "seerr-configure.service" ];
      };

      runtimeTemplates."seerr.env" = {
        content = ''
          API_KEY=${config.selfhost.runtimePlaceholder.seerr-api-key}
        '';
        restartUnits = [ "seerr.service" ];
      };
    };

    services.seerr = {
      enable = true;
      inherit (serviceCfg) port;
    };

    systemd.services.seerr.serviceConfig.EnvironmentFile = config.selfhost.runtimeTemplates."seerr.env".path;
  };
}
