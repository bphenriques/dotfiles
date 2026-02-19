{ config, lib, ... }:
let
  cfg = config.custom.home-server;
  serviceCfg = cfg.services.homepage;
  dashboardCfg = cfg.dashboard;

  # Generate services.yaml from custom.home-server.services
  mkServiceEntry = service: {
    "${service.name}" = {
      href = service.publicUrl;
      description = service.dashboard.description;
      icon = service.dashboard.icon;
    }
    // lib.optionalAttrs service.dashboard.siteMonitor {
      siteMonitor = service.publicUrl;
    };
    # TODO: widget support
  };

  mkCategorySection = category: services: {
    "${category}" = map mkServiceEntry services;
  };

  servicesYaml =
    let
      orderedCategories = lib.filter (
        cat: lib.hasAttr cat cfg._dashboardServicesByCategory
      ) dashboardCfg.categoryOrder;
      sections = map (
        cat: mkCategorySection cat cfg._dashboardServicesByCategory.${cat}
      ) orderedCategories;
    in
    lib.flatten sections;

  # Generate layout from categories
  mkLayoutEntry = category: {
    "${category}" = {
      tab = category;
      style = "row";
      columns = 4;
      header = false;
      useEqualHeights = true;
    };
  };

  defaultSettings = {
    title = "Home";
    language = "en-GB";
    background = "/images/background.png";
    favicon = "/images/favicon.ico";
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
    layout = lib.listToAttrs (
      map (cat: lib.nameValuePair cat (mkLayoutEntry cat).${cat}) dashboardCfg.categoryOrder
    );
  };
in
{
  custom.home-server.services.homepage = {
    port = 3001;
    forwardAuth.enable = false;
  };

  custom.home-server.dashboard = {
    settings = { };
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

  services.homepage-dashboard = {
    enable = true;
    listenPort = serviceCfg.port;
    settings = defaultSettings // dashboardCfg.settings;
    services = servicesYaml;
    widgets = dashboardCfg.widgets;
    bookmarks = dashboardCfg.bookmarks;
    allowedHosts = serviceCfg.publicHost;
  };
}
