{ config, pkgs, self, ... }:
let
  wallpaper = self.packages.wallpapers.files.sky-sunset;
  favicon = ./compass.svg;
in
{
  selfhost.dashboards.homepage.enable = true;
  services.homepage-dashboard = {
    package = pkgs.homepage-dashboard.overrideAttrs (oldAttrs: {
      postInstall = (oldAttrs.postInstall or "") + ''
        mkdir -p $out/share/homepage/public/images
        ln -s ${wallpaper} $out/share/homepage/public/images/background.png
        ln -s ${favicon} $out/share/homepage/public/images/favicon.svg
      '';
    });
    settings = {
      title = "Home";
      language = "en-GB";
      theme = "dark";
      color = "stone";
      statusStyle = "dot";
      headerStyle = "clean";
      hideVersion = true;
      target = "_blank";
      background = "/images/background.png";
      favicon = "/images/favicon.svg";
      quicklaunch = {
        searchDescriptions = true;
        hideInternetSearch = true;
        hideVisitURL = true;
      };
      layout = [
        { "Services"  = { tab = "Home";   style = "row";     columns = 6; header = false; }; }
        { "Admin"     = { tab = "Admin";  style = "columns"; columns = 6; header = false; }; }
      ];
    };

    widgets = [
      {
        resources = {
          cpu = true;
          memory = true;
          cputemp = true;
          tempmin = 30;
          tempmax = 105;
          disk = "/";
          units = "metric";
          label = " ";
        };
      }
      {
        openmeteo = {
          inherit (config.custom.locale) latitude longitude timezone;
          label = "Lisbon";
          units = "metric";
          cache = 300;
        };
      }
    ];
  };
}
