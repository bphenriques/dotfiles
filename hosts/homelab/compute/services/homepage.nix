{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.homepage;
  generatedServices = cfg.homepage.generatedServices;

  # Display configuration
  categoryOrder = [ "Media" "Productivity" "Admin" "Infrastructure" "Development" ];
  adminCategories = [ "Admin" "Infrastructure" ];

  isAdminCategory = cat: lib.elem cat adminCategories;
  displayCategory = cat: if isAdminCategory cat then "Admin" else "Home";

  # Build services.yaml from generated services, merging by display category
  getServicesForCategories = cats: lib.concatLists (map (cat:
    if lib.hasAttr cat generatedServices then generatedServices.${cat} else []
  ) cats);

  homeServices = getServicesForCategories (lib.filter (c: !isAdminCategory c) categoryOrder);
  adminServicesYaml = getServicesForCategories (lib.filter isAdminCategory categoryOrder);

  servicesYaml =
    (lib.optional (homeServices != []) { "Home" = homeServices; })
    ++ (lib.optional (adminServicesYaml != []) { "Admin" = adminServicesYaml; });

  # Custom package with wallpaper/favicon
  wallpaper = "${self.pkgs.wallpapers}/share/wallpapers/sky-sunset.png";
  favicon = self.lib.builders.mkEmojiFavicon { } "homepage" "🏠";
  customPackage = pkgs.homepage-dashboard.overrideAttrs (oldAttrs: {
    postInstall = (oldAttrs.postInstall or "") + ''
      mkdir -p $out/share/homepage/public/images
      ln -s ${wallpaper} $out/share/homepage/public/images/background.png
      ln -s ${favicon} $out/share/homepage/public/images/favicon.ico
    '';
  });
in
{
  custom.homelab.services.homepage = {
    port = 3001;
    forwardAuth.enable = false;
    integrations.homepage = {
      enable = true;
      category = "Infrastructure";
      description = "Dashboard";
    };
  };

  services.homepage-dashboard = {
    enable = true;
    package = customPackage;
    listenPort = serviceCfg.port;
    allowedHosts = serviceCfg.publicHost;
    services = servicesYaml;

    settings = {
      title = "Home";
      language = "en-GB";
      theme = "dark";
      color = "stone";
      statusStyle = "dot";
      headerStyle = "clean";
      hideVersion = true;
      target = "_self";
      background = "/images/background.png";
      favicon = "/images/favicon.ico";
      quicklaunch = {
        searchDescriptions = true;
        hideInternetSearch = true;
        hideVisitURL = true;
      };
      layout = [
        {
          "Home" = {
            tab = "Home";
            style = "row";
            columns = 4;
            header = false;
            useEqualHeights = true;
          };
        }
        {
          "Admin" = {
            tab = "Admin";
            style = "column";
            columns = 4;
            header = false;
            useEqualHeights = true;
          };
        }
      ];
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
