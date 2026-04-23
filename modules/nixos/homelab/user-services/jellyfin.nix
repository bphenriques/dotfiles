{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    (_: {
      options.services.jellyfin = {
        enable = lib.mkEnableOption "Jellyfin account for this user";
        # FIXME: Remove once Seerr supports OIDC - used for local Jellyfin auth
        passwordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to file containing local Jellyfin password (for Seerr auth until OIDC is supported)";
        };
      };
    })
  ];
}
