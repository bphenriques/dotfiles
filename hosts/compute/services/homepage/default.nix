{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.homepage;
  sonarrCfg = cfg.services.sonarr;
  radarrCfg = cfg.services.radarr;
  generatedServices = cfg.homepage.generatedServices;

  # Display order for categories within each tab
  categoryOrder = [ "General" "Media" "Monitoring" "Administration" ];

  sortedCategories = tabServices: let
    known = lib.filter (c: lib.hasAttr c tabServices) categoryOrder;
    extra = lib.filter (c: !lib.elem c categoryOrder) (lib.attrNames tabServices);
  in known ++ extra;

  homeCategories = sortedCategories generatedServices.Home;
  adminCategories' = sortedCategories generatedServices.Admin;

  mkCategoryGroup = tabServices: cat: let svcs = tabServices.${cat} or []; in
    lib.optional (svcs != []) { "${cat}" = svcs; };

  agendaGroup = {
    "Movie/TV Agenda" = [{
      "Movie/TV Agenda" = {
        widget = {
          type = "calendar";
          view = "agenda";
          maxEvents = 10;
          showTime = true;
          previousDays = 3;
          timezone = config.time.timeZone;
          integrations = [
            {
              type = "sonarr";
              service_group = "Admin";
              service_name = "sonarr";
            }
            {
              type = "radarr";
              service_group = "Admin";
              service_name = "radarr";
            }
          ];
        };
      };
    }];
  };

  adminServices = lib.concatLists (map (cat: generatedServices.Admin.${cat} or []) adminCategories');

  servicesYaml =
    lib.concatMap (mkCategoryGroup generatedServices.Home) homeCategories
    ++ [ agendaGroup ]
    ++ lib.optional (adminServices != []) { "Admin" = adminServices; };

  # Custom package with wallpaper/favicon
  wallpaper = "${self.pkgs.wallpapers}/share/wallpapers/sky-sunset.png";
  favicon = ./compass.svg;
  customPackage = pkgs.homepage-dashboard.overrideAttrs (oldAttrs: {
    postInstall = (oldAttrs.postInstall or "") + ''
      mkdir -p $out/share/homepage/public/images
      ln -s ${wallpaper} $out/share/homepage/public/images/background.png
      ln -s ${favicon} $out/share/homepage/public/images/favicon.svg
    '';
  });
in
{
  # Does not require authentication as it just aggregates links and runs health checks behind the scenes.
  custom.homelab.services.homepage = {
    description = "Dashboard";
    version = config.services.homepage-dashboard.package.version;
    homepage = config.services.homepage-dashboard.package.meta.homepage;
    category = "Infrastructure";
    port = 3001;
    secrets = {
      templates."homepage.env".content = lib.concatStringsSep "\n" [
        "HOMEPAGE_VAR_SONARR_API_KEY=${sonarrCfg.secrets.placeholder.api-key}"
        "HOMEPAGE_VAR_RADARR_API_KEY=${radarrCfg.secrets.placeholder.api-key}"
      ];
      systemd.dependentServices = [ "homepage-dashboard" ];
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
      favicon = "/images/favicon.svg";
      quicklaunch = {
        searchDescriptions = true;
        hideInternetSearch = true;
        hideVisitURL = true;
      };
      layout =
        map (cat: {
          "${cat}" = {
            tab = "Home";
            style = "columns";
            columns = 6;
            header = false;
            useEqualHeights = false;
          };
        }) homeCategories
        ++ [{
          "Movie/TV Agenda" = {
            tab = "Home";
            style = "columns";
            columns = 3;
            header = false;
          };
        }]
        ++ [{
          "Admin" = {
            tab = "Admin";
            style = "row";
            columns = 6;
            header = false;
            useEqualHeights = true;
          };
        }];
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
          label = " "; # Intentionally blank space to ensure we have some vertical margin
        };
      }
      {
        openmeteo = {
          label = "Lisbon";
          latitude = 38.736946;
          longitude = -9.142685;
          timezone = config.time.timeZone;
          units = "metric";
          cache = 300;
        };
      }

    ];
  };

  services.homepage-dashboard.environmentFiles = [
    serviceCfg.secrets.templates."homepage.env".path
  ];
}
