{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.homepage;
  sonarrCfg = cfg.services.sonarr;
  radarrCfg = cfg.services.radarr;
  homepageCfg = cfg.homepage;

  # Custom package with wallpaper/favicon
  wallpaper = self.packages.wallpapers.files.sky-sunset;
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
    bookmarks = [ ]; # No use yet, but considering.
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
              inherit (cfg.locale) timezone;
              integrations = [
                { type = "sonarr"; service_group = "Services"; service_name = sonarrCfg.displayName; }
                { type = "radarr"; service_group = "Services"; service_name = radarrCfg.displayName; }
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
      target = "_blank";
      background = "/images/background.png";
      favicon = "/images/favicon.svg";
      quicklaunch = {
        searchDescriptions = true;
        hideInternetSearch = true;
        hideVisitURL = true;
      };
      layout = [
        { "Bookmarks" = { tab = "Home"; style = "row"; columns = 8; header = false; }; }
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
        openmeteo = { # City location
          label = "Lisbon";
          inherit (cfg.locale) latitude;
          inherit (cfg.locale) longitude;
          inherit (cfg.locale) timezone;
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
