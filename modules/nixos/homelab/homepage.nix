# Homepage Dashboard Module
#
# Consumes the service registry and configures the homepage-dashboard service.
# Generates services.yaml from registered services with dashboard metadata.
#
{ lib, config, ... }:
let
  cfg = config.custom.homelab;
  homepageCfg = cfg.homepage;
  registry = cfg._registry;

  categoryType = lib.types.enum [
    "Media"
    "Admin"
    "Productivity"
    "Development"
    "Infrastructure"
  ];

  mkServiceEntry = service: {
    "${service.name}" = {
      href = service.publicUrl;
      description = service.dashboard.description;
      icon = service.dashboard.icon;
    } // lib.optionalAttrs service.dashboard.siteMonitor {
      siteMonitor = service.publicUrl;
    };
  };

  mkCategorySection = category: services: {
    "${category}" = map mkServiceEntry services;
  };

  servicesYaml = let
    orderedCategories = lib.filter (
      cat: lib.hasAttr cat registry.servicesByCategory
    ) homepageCfg.categoryOrder;
    sections = map (
      cat: mkCategorySection cat registry.servicesByCategory.${cat}
    ) orderedCategories;
  in lib.flatten sections;

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
      map (cat: lib.nameValuePair cat (mkLayoutEntry cat).${cat}) homepageCfg.categoryOrder
    );
  };
in
{
  options.custom.homelab.homepage = {
    enable = lib.mkEnableOption "homepage dashboard";

    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = "Additional homepage settings.yaml content (merged with defaults)";
    };

    widgets = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = [ ];
      description = "Homepage widgets.yaml content";
    };

    bookmarks = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = [ ];
      description = "Homepage bookmarks.yaml content";
    };

    categoryOrder = lib.mkOption {
      type = lib.types.listOf categoryType;
      default = [ "Media" "Productivity" "Admin" "Infrastructure" "Development" ];
      description = "Order of categories displayed in dashboard";
    };
  };

  config = lib.mkIf (cfg.enable && homepageCfg.enable) (let
    serviceCfg = cfg.services.homepage;
  in {
    custom.homelab.services.homepage = {
      port = lib.mkDefault 3001;
      forwardAuth.enable = lib.mkDefault false;
    };

    services.homepage-dashboard = {
      enable = true;
      listenPort = serviceCfg.port;
      settings = defaultSettings // homepageCfg.settings;
      services = servicesYaml;
      widgets = homepageCfg.widgets;
      bookmarks = homepageCfg.bookmarks;
      allowedHosts = serviceCfg.publicHost;
    };
  });
}
