{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ ... }: {
      options.services.jellyseerr = {
        enable = lib.mkEnableOption "Jellyseerr account for this user (requires Jellyfin)";
        permissions = {
          autoApprove = lib.mkEnableOption "auto-approve requests";
          advancedRequests = lib.mkEnableOption "advanced request options (e.g., quality profile)";
          viewRecentlyAdded = lib.mkEnableOption "view recently added media";
        };
      };
    })
  ];
}
