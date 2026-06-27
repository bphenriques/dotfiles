{ config, lib, pkgs, self, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.homepage;

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
  selfhost.services.homepage = {
    description = "Dashboard";
    port = 3001;
    integrations.homepage.enable = false; # The dashboard doesn't list itself.
  };

  selfhost.runtimeTemplates."homepage.env" = {
    content = lib.concatStringsSep "\n" [
      "HOMEPAGE_VAR_SONARR_API_KEY=${cfg.runtimePlaceholder.sonarr-api-key}"
      "HOMEPAGE_VAR_RADARR_API_KEY=${cfg.runtimePlaceholder.radarr-api-key}"
    ];
    restartUnits = [ "homepage-dashboard.service" ];
  };

  services.homepage-dashboard = {
    enable = true;
    listenPort = serviceCfg.port;
    allowedHosts = serviceCfg.publicHost;
    package = customPackage;
    bookmarks = [ ];

    # Registry tiles + our custom calendar group; order/tabs come from settings.layout below.
    services =
      lib.mapAttrsToList (group: tiles: { ${group} = tiles; }) cfg.dashboards.generatedTiles
      ++ [{
        "Movie/TV Agenda" = [{
          "" = {
            widget = {
              type = "calendar";
              view = "agenda";
              maxEvents = 10;
              showTime = true;
              previousDays = 3;
              inherit (config.custom.locale) timezone;
              integrations = [
                { type = "sonarr"; service_group = "Services"; service_name = cfg.services.sonarr.displayName; }
                { type = "radarr"; service_group = "Services"; service_name = cfg.services.radarr.displayName; }
              ];
            };
          };
        }];
      }];

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
          inherit (config.custom.locale) latitude;
          inherit (config.custom.locale) longitude;
          inherit (config.custom.locale) timezone;
          units = "metric";
          cache = 300;
        };
      }
    ];

    environmentFiles = [
      cfg.runtimeTemplates."homepage.env".path
    ];
  };
}
