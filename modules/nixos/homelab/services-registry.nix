{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  categoryType = lib.types.enum [
    "Media"
    "Admin"
    "Productivity"
    "Development"
    "Infrastructure"
  ];

  dashboardOpts = lib.types.submodule {
    options = {
      enable = lib.mkEnableOption "dashboard entry for this service";

      category = lib.mkOption {
        type = categoryType;
        description = "Dashboard category/tab for this service";
      };

      description = lib.mkOption {
        type = lib.types.str;
        description = "Short description shown on dashboard";
      };

      icon = lib.mkOption {
        type = lib.types.str;
        description = "Icon name from dashboard-icons (e.g. 'miniflux.svg')";
      };

      siteMonitor = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable health monitoring for this service";
      };
    };
  };

  serviceOpt = lib.types.submodule ({ name, config, ... }: {
    options = {
      # Identity
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Service identifier (defaults to attribute name)";
      };

      # Routing
      internalHost = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "Internal hostname or IP where the service listens";
      };

      internalUrl = lib.mkOption {
        type = lib.types.str;
        default = "http://${config.internalHost}:${toString config.port}";
        description = "Full internal URL for proxying (derived from internalHost and port)";
      };

      subdomain = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Subdomain prefix (combined with domain for publicHost)";
      };

      publicHost = lib.mkOption {
        type = lib.types.str;
        default = "${config.subdomain}.${cfg.domain}";
        description = "Public hostname (derived from subdomain and domain)";
      };

      publicUrl = lib.mkOption {
        type = lib.types.str;
        default = "https://${config.publicHost}";
        description = "Full public URL (derived from publicHost)";
      };

      port = lib.mkOption {
        type = lib.types.port;
        description = "Port the service listens on";
      };

      # Ingress-level authentication (Traefik forwardAuth)
      forwardAuth = {
        enable = lib.mkEnableOption ''
          ingress-level access control via Traefik forwardAuth.
          Independent of app-level SSO (OIDC) the service might use
        '';

        group = lib.mkOption {
          type = lib.types.enum [ cfg.groups.admin cfg.groups.users ];
          default = cfg.groups.admin;
          description = "Group required to access this service via forwardAuth (admin or users)";
        };
      };

      # Dashboard/UI metadata
      dashboard = lib.mkOption {
        type = lib.types.nullOr dashboardOpts;
        default = null;
        description = "Dashboard configuration for homepage (presentation metadata)";
      };
    };
  });

  # Derived views for consumers
  allServices = lib.attrValues cfg.services;
  dashboardServices = lib.filter (s: s.dashboard != null && s.dashboard.enable) allServices;
  servicesByCategory = lib.groupBy (s: s.dashboard.category) dashboardServices;
in
{
  options.custom.homelab = {
    enable = lib.mkEnableOption "home-server services";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "Base domain for all services (e.g. 'home.example.com')";
    };

    services = lib.mkOption {
      type = lib.types.attrsOf serviceOpt;
      default = { };
      description = ''
        Registry of HTTP services: routing (host/port) and optional dashboard/UI metadata.
        Each service gets a subdomain under the base domain.
      '';
    };

    # Read-only derived views for consumers
    _registry = {
      allServices = lib.mkOption {
        type = lib.types.listOf serviceOpt;
        default = allServices;
        internal = true;
        readOnly = true;
        description = "All registered services as a list";
      };

      dashboardServices = lib.mkOption {
        type = lib.types.listOf serviceOpt;
        default = dashboardServices;
        internal = true;
        readOnly = true;
        description = "Services with dashboard enabled";
      };

      servicesByCategory = lib.mkOption {
        type = lib.types.attrsOf (lib.types.listOf serviceOpt);
        default = servicesByCategory;
        internal = true;
        readOnly = true;
        description = "Dashboard services grouped by category";
      };
    };

  };

  config = lib.mkIf cfg.enable {
    assertions = let
      urls = map (s: s.internalUrl) allServices;
      duplicates = lib.filter (url: lib.count (u: u == url) urls > 1) (lib.unique urls);
    in [
      {
        assertion = duplicates == [ ];
        message = "Service URLs must be unique. Conflicting: ${toString duplicates}";
      }
    ];
  };
}
