# Homepage Dashboard Configuration
#
# Host-specific settings for the homepage dashboard.
# The actual homepage-dashboard service is configured by modules/nixos/homelab/homepage.nix
#
{ self, ... }:
let
  wallpaper = "${self.pkgs.wallpapers}/share/wallpapers/sky-sunset.png";
  favicon = self.lib.builders.mkEmojiFavicon { } "homepage" "🏠";
in
{
  custom.homelab.homepage = {
    enable = true;
    inherit wallpaper favicon;
    settings = {
      title = "Home";
      language = "en-GB";
      theme = "dark";
      color = "stone";
      statusStyle = "dot";
      headerStyle = "clean";
      hideVersion = true;
      target = "_self";
      quicklaunch = {
        searchDescriptions = true;
        hideInternetSearch = true;
        hideVisitURL = true;
      };
    };
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
  };
}
