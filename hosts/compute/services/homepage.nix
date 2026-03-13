{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.homepage;
  generatedServices = cfg.homepage.generatedServices;

  # Display order and tab assignment
  categoryOrder = [ "General" "Media" "Monitoring" "Administration" ];
  adminCategories = [ "Monitoring" "Administration" ];
  isAdminCategory = cat: lib.elem cat adminCategories;

  # Order categories: explicit order first, then any unlisted ones
  allCategories = let
    known = lib.filter (c: lib.hasAttr c generatedServices) categoryOrder;
    extra = lib.filter (c: !lib.elem c categoryOrder) (lib.attrNames generatedServices);
  in known ++ extra;
  getServices = cats: lib.concatLists (map (cat: generatedServices.${cat} or []) cats);

  homeServices = getServices (lib.filter (c: !isAdminCategory c) allCategories);
  adminServicesYaml = getServices (lib.filter isAdminCategory allCategories);

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
    description = "Dashboard";
    version = config.services.homepage-dashboard.package.version;
    homepage = config.services.homepage-dashboard.package.meta.homepage;
    category = "Infrastructure";
    port = 3001;
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
        resources = {
          cpu = true;
          memory = true;
          disk = "/";
          label = "System";
        };
      }
      {
        openmeteo = {
          label = "Lisbon";
          latitude = 38.736946;
          longitude = -9.142685;
          timezone = "Europe/Lisbon";
          units = "metric";
          cache = 300;
        };
      }
    ];
  };
}
