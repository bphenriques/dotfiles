# Homepage Dashboard Module
#
# Consumes the service registry and configures the homepage-dashboard service.
# Generates services.yaml from registered services with dashboard metadata.
#
{ lib, config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  homepageCfg = cfg.homepage;

  # Homepage-specific derived views
  homepageServices = lib.filter (s: s.integrations.homepage != null && s.integrations.homepage.enable) cfg.registry.allServices;
  servicesByCategory = lib.groupBy (s: s.integrations.homepage.category) homepageServices;

  customPackage = pkgs.homepage-dashboard.overrideAttrs (oldAttrs: {
    postInstall = (oldAttrs.postInstall or "") + ''
      mkdir -p $out/share/homepage/public/images
      ln -s ${homepageCfg.wallpaper} $out/share/homepage/public/images/background.png
      ln -s ${homepageCfg.favicon} $out/share/homepage/public/images/favicon.ico
    '';
  });

  categoryType = lib.types.enum [
    "Media"
    "Admin"
    "Productivity"
    "Development"
    "Infrastructure"
  ];

  adminCategories = [ "Admin" "Infrastructure" ];

  mkServiceEntry = service: {
    "${service.name}" = {
      href = service.publicUrl;
      description = service.integrations.homepage.description;
      icon = service.integrations.homepage.icon;
    } // lib.optionalAttrs service.integrations.homepage.siteMonitor {
      siteMonitor = service.publicUrl;
    };
  };

  mkCategorySection = category: services: {
    "${category}" = map mkServiceEntry services;
  };

  servicesYaml = let
    orderedCategories = lib.filter (
      cat: lib.hasAttr cat servicesByCategory
    ) homepageCfg.categoryOrder;
    sections = map (
      cat: mkCategorySection (if isAdminCategory cat then "Admin" else "Home") servicesByCategory.${cat}
    ) orderedCategories;
  in lib.flatten sections;

  isAdminCategory = category: lib.elem category adminCategories;

  defaultSettings = {
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

    wallpaper = lib.mkOption {
      type = lib.types.path;
      description = "Path to wallpaper image for background";
    };

    favicon = lib.mkOption {
      type = lib.types.path;
      description = "Path to favicon file";
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
      package = customPackage;
      listenPort = serviceCfg.port;
      settings = defaultSettings // homepageCfg.settings // {
        background = "/images/background.png";
        favicon = "/images/favicon.ico";
      };
      services = servicesYaml;
      widgets = homepageCfg.widgets;
      bookmarks = homepageCfg.bookmarks;
      allowedHosts = serviceCfg.publicHost;
    };
  });
}
