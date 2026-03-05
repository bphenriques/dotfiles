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

  mkHomepageOpts = serviceName: lib.types.submodule {
    options = {
      enable = lib.mkEnableOption "homepage entry for this service";

      category = lib.mkOption {
        type = categoryType;
        description = "Homepage category/tab for this service";
      };

      description = lib.mkOption {
        type = lib.types.str;
        description = "Short description shown on homepage";
      };

      icon = lib.mkOption {
        type = lib.types.str;
        default = "${serviceName}.svg";
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

      # Routing (backend)
      host = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "Hostname or IP where the service listens (local or remote)";
      };

      port = lib.mkOption {
        type = lib.types.port;
        description = "Port the service listens on";
      };

      scheme = lib.mkOption {
        type = lib.types.enum [ "http" "https" ];
        default = "http";
        description = "URL scheme for backend connection";
      };

      url = lib.mkOption {
        type = lib.types.str;
        default = "${config.scheme}://${config.host}:${toString config.port}";
        description = "Full URL for proxying (derived from scheme, host and port)";
      };

      # Routing (public)
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

      # Ingress-level authentication (Traefik forwardAuth)
      forwardAuth = {
        enable = lib.mkEnableOption ''
          ingress-level access control via Traefik forwardAuth.
          Independent of app-level SSO (OIDC) the service might use
        '';

        # Values are actual group names from cfg.groups (not fixed strings "admin"/"users").
        # This allows changing group names in one place (custom.homelab.groups).
        group = lib.mkOption {
          type = lib.types.enum [ cfg.groups.admin cfg.groups.users ];
          default = cfg.groups.admin;
          description = "Group required to access this service via forwardAuth (uses group names from custom.homelab.groups)";
        };
      };

      # Integrations with external tools
      integrations = {
        homepage = lib.mkOption {
          type = lib.types.nullOr (mkHomepageOpts name);
          default = null;
          description = "Homepage dashboard integration";
        };
      };

      # Per-service secrets (contract from _secrets-schema.nix)
      secrets = lib.mkOption {
        type = lib.types.submodule (import ./_secrets-schema.nix {
          inherit lib;
          serviceName = name;
        });
        default = { };
        description = "Generated secrets for this service";
      };

      # Per-service OIDC client (contract from _oidc-schema.nix)
      oidc = lib.mkOption {
        type = lib.types.submodule (import ./_oidc-schema.nix {
          inherit lib;
          serviceName = name;
          serviceConfig = config;
        });
        default = { };
        description = "OIDC client configuration for this service";
      };
    };
  });
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
        Registry of HTTP services: routing, secrets, OIDC, and integrations.
        Each service gets a subdomain under the base domain.
      '';
    };

    # Read-only derived views for consumers
    registry = {
      allServices = lib.mkOption {
        type = lib.types.listOf serviceOpt;
        default = lib.attrValues cfg.services;
        readOnly = true;
        description = "All registered services as a list";
      };
    };

  };

  config = lib.mkIf cfg.enable {
    assertions = let
      allServices = lib.attrValues cfg.services;
      urls = map (s: s.url) allServices;
      duplicates = lib.filter (url: lib.count (u: u == url) urls > 1) (lib.unique urls);
    in [
      {
        assertion = duplicates == [ ];
        message = "Service URLs must be unique. Conflicting: ${toString duplicates}";
      }
    ];
  };
}
