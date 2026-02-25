{ lib, config, ... }:
let
  cfg = config.custom.home-server;

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

      # TODO: Widget support
      # widget = lib.mkOption { ... };
    };
  };

  serviceOpt = lib.types.submodule ({ name, config, ... }: {
    options = {
      # Identity
      name = lib.mkOption { type = lib.types.str; default = name; };

      # Routing
      internalHost = lib.mkOption { type = lib.types.str; default = "127.0.0.1"; };
      internalUrl = lib.mkOption { type = lib.types.str; default = "http://${config.internalHost}:${toString config.port}"; };
      subdomain = lib.mkOption { type = lib.types.str; default = name; };
      publicHost = lib.mkOption { type = lib.types.str; default = "${config.subdomain}.${cfg.domain}"; };
      publicUrl = lib.mkOption { type = lib.types.str; default = "https://${config.publicHost}"; };
      port = lib.mkOption { type = lib.types.port; };

      # Authentication
      forwardAuth = {
        enable = lib.mkEnableOption "forward authentication via tinyauth";
        group = lib.mkOption {
          type = lib.types.str;
          default = "admin";
          description = "Group required to access this service";
        };
      };

      # Dashboard
      dashboard = lib.mkOption {
        type = lib.types.nullOr dashboardOpts;
        default = null;
        description = "Dashboard configuration for homepage";
      };
    };
  });

  mkTraefikRoute = service: {
    http = {
      routers."${service.name}" = {
        rule = "Host(`${service.publicHost}`)";
        entryPoints = [ "websecure" ];
        service = "${service.name}-svc";
      } // lib.optionalAttrs service.forwardAuth.enable {
        middlewares = [ "tinyauth" ];
      };
      services."${service.name}-svc".loadBalancer.servers = [{ url = service.internalUrl; }];
    };
  };

  usedGroups = lib.unique (
    map (s: s.forwardAuth.group)
    (lib.filter (s: s.forwardAuth.enable) (lib.attrValues cfg.services))
  );

  # Services with dashboard enabled, grouped by category
  dashboardServices = lib.filter (s: s.dashboard != null && s.dashboard.enable) (lib.attrValues cfg.services);
  servicesByCategory = lib.groupBy (s: s.dashboard.category) dashboardServices;
in
{
  options.custom.home-server = {
    enable = lib.mkEnableOption "Home-server service";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "Base domain for all services";
    };

    cloudflareEmail = lib.mkOption {
      type = lib.types.str;
      description = "Cloudflare account email for DNS challenge";
    };

    services = lib.mkOption {
      type = lib.types.attrsOf serviceOpt;
      default = { };
      description = "Home server services configuration";
    };

    forwardAuth = {
      enable = lib.mkEnableOption "Forward authentication service";
      internalUrl = lib.mkOption {
        type = lib.types.str;
        description = "Internal URL for the forward auth service";
      };
    };

    dashboard = {
      settings = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
        description = "Homepage settings.yaml content";
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
        description = "Order of categories in dashboard";
      };
    };

    # Expose grouped services for homepage module
    _dashboardServicesByCategory = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf serviceOpt);
      default = servicesByCategory;
      internal = true;
      readOnly = true;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = let
      allServices = lib.attrValues cfg.services;
      urls = map (s: s.internalUrl) allServices;
      duplicates = lib.subtractLists (lib.unique urls) urls;
      invalidGroups = lib.subtractLists cfg.groups.allowed usedGroups;
    in [
      {
        assertion = (builtins.length duplicates) == 0;
        message = "Service URLs must be unique. Conflicting: ${toString duplicates}";
      }
      {
        assertion = (builtins.length invalidGroups) == 0;
        message = "Invalid forwardAuth groups: ${toString invalidGroups}. Allowed: ${toString cfg.groups.allowed}";
      }
    ];

    networking.firewall = {
      allowedTCPPorts = [ 80 443 ];
      allowedUDPPorts = [ 443 ];
    };

    sops = {
      secrets.cloudflare_dns_api_token = { };
      templates."home-server" = {
        owner = "traefik";
        content = ''
          CF_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_dns_api_token}
        '';
      };
    };

    systemd.services.traefik = {
      serviceConfig.EnvironmentFile = config.sops.templates.home-server.path;
      environment.CF_API_EMAIL = cfg.cloudflareEmail;
    };

    services.traefik = {
      enable = true;
      staticConfigOptions = {
        log.level = "ERROR";
        entryPoints = {
          web = {
            address = ":80";
            http.redirections.entryPoint = {
              to = "websecure";
              scheme = "https";
            };
          };

          websecure = {
            address = ":443";
            http.tls = {
              certResolver = "default";
              domains = [{
                main = cfg.domain;
                sans = [ "*.${cfg.domain}" ];
              }];
            };

            transport.respondingTimeouts = {
              readTimeout = "300s";
              writeTimeout = "300s";
            };
          };
        };

        certificatesResolvers.default.acme = {
          email = cfg.cloudflareEmail;
          storage = "/var/lib/traefik/acme.json";
          dnsChallenge.provider = "cloudflare";
        };
      };

      dynamicConfigOptions = let
        routeConfigs = lib.mapAttrsToList (_: mkTraefikRoute) cfg.services;
        routesConfig = lib.foldl' lib.recursiveUpdate { } routeConfigs;
        authMiddleware = lib.optionalAttrs cfg.forwardAuth.enable {
          http.middlewares.tinyauth.forwardAuth = {
            address = "${cfg.forwardAuth.internalUrl}/api/auth/traefik";
            trustForwardHeader = true;
            authResponseHeaders = [
              "X-Forwarded-User" # TODO READ WHY THIS IS REQUIRED AGAIN
              "X-Forwarded-Groups"
              "X-Forwarded-Email"
            ];
          };
        };
      in lib.foldl' lib.recursiveUpdate { } [ routesConfig authMiddleware ];
    };
  };
}
