{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.homepage;
  sonarrCfg = cfg.services.sonarr;
  radarrCfg = cfg.services.radarr;
  homepageCfg = cfg.homepage;

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
  # Unauthenticated: aggregates links and health status.
  custom.homelab.services.homepage = {
    metadata.description = "Dashboard";
    metadata.version = config.services.homepage-dashboard.package.version;
    metadata.homepage = config.services.homepage-dashboard.package.meta.homepage;
    metadata.category = "Infrastructure";
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
    services =
      lib.optional (homepageCfg.generatedHomeServices != []) { "Services" = homepageCfg.generatedHomeServices; }
      ++ [{
        "Movie/TV Agenda" = [{
          "" = {
            widget = {
              type = "calendar";
              view = "agenda";
              maxEvents = 10;
              showTime = true;
              previousDays = 3;
              timezone = config.time.timeZone;
              integrations = [
                { type = "sonarr"; service_group = "Admin"; service_name = "sonarr"; }
                { type = "radarr"; service_group = "Admin"; service_name = "radarr"; }
              ];
            };
          };
        }];
      }]
      ++ lib.optional (homepageCfg.generatedAdminServices != []) { "Admin" = homepageCfg.generatedAdminServices; };

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
      layout = [
        { "Services" = { tab = "Home"; style = "row"; columns = 6; header = false; }; }
        { "Movie/TV Agenda" = { tab = "Home"; style = "row"; columns = 3; header = false; }; }
        { "Admin" = { tab = "Admin"; style = "columns"; columns = 6; header = false; useEqualHeights = true; }; }
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
          label = "Lisbon";
          latitude = 38.736946;
          longitude = -9.142685;
          timezone = config.time.timeZone;
          units = "metric";
          cache = 300;
        };
      }
    ];

    environmentFiles = [
      serviceCfg.secrets.templates."homepage.env".path
    ];
  };
}
