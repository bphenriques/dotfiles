# Homepage Dashboard Configuration
#
# Host-specific settings for the homepage dashboard.
# The actual homepage-dashboard service is configured by modules/nixos/home-server/homepage.nix
#
{ ... }:
{
  custom.homelab.homepage = {
    enable = true;
    widgets = [
      {
        openmeteo = {
          label = "Lisbon";
          latitude = 38.736946;
          longitude = -9.142685;
          timezone = "Europe/Lisbon";
          units = "metric";
          cache = 30;
        };
      }
    ];
    bookmarks = [ ];
  };
}
