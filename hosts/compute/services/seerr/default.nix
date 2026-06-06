{ config, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.seerr;
in
{
  imports = [ ./configure.nix ];

  options.custom.homelab.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.seerr = {
        enable = lib.mkEnableOption "Seerr account for this user (requires Jellyfin)";
        permissions = {
          autoApprove = lib.mkEnableOption "auto-approve requests";
          advancedRequests = lib.mkEnableOption "advanced request options (e.g., quality profile)";
          viewRecentlyAdded = lib.mkEnableOption "view recently added media";
        };
      };
    });
  };

  # Auth is delegated to Jellyfin (users sign in via Jellyfin credentials, not direct OIDC/forwardAuth)
  config = {
    custom.homelab = {
      services.seerr = {
        displayName = "Seerr";
        description = "TV / Movie Finder";
        port = 9099;
        healthcheck.path = "/api/v1/status";
        integrations.homepage.enable = true;
      };

      # Upstream uses DynamicUser; EnvironmentFile loads as root before user drop.
      runtimeSecrets.seerr-api-key = {
        restartUnits = [ "seerr.service" "seerr-configure.service" ];
      };

      runtimeTemplates."seerr.env" = {
        content = ''
          API_KEY=${config.custom.homelab.runtimePlaceholder.seerr-api-key}
        '';
        restartUnits = [ "seerr.service" ];
      };
    };

    services.seerr = {
      enable = true;
      inherit (serviceCfg) port;
    };

    systemd.services.seerr.serviceConfig.EnvironmentFile = config.custom.homelab.runtimeTemplates."seerr.env".path;
  };
}
