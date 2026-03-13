{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ ... }: {
      options.services.jellyfin = {
        enable = lib.mkEnableOption "Jellyfin account for this user";
        # FIXME: Remove once Jellyseerr supports OIDC - used for local Jellyfin auth
        passwordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to file containing local Jellyfin password (for Jellyseerr auth until OIDC is supported)";
        };
      };
    })
  ];
}
