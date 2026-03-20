{ lib, config, ... }:
let
  cfg = config.custom.homelab;
  categoryType = lib.types.enum [
    "General"
    "Media"
    "Monitoring"
    "Administration"
    "Infrastructure"
  ];

  baseServiceModule = { name, config, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Service identifier (defaults to attribute name)";
      };

      metadata = lib.mkOption {
        type = lib.types.submodule {
          options = {
            description = lib.mkOption {
              type = lib.types.str;
              description = "Short description of the service";
            };

            category = lib.mkOption {
              type = categoryType;
              description = "Service category for catalogue and homepage grouping";
            };

            version = lib.mkOption {
              type = lib.types.str;
              description = "Service version (e.g. '4.7.0')";
            };

            homepage = lib.mkOption {
              type = lib.types.str;
              description = "Upstream project URL";
            };
          };
        };
        description = "Service metadata: description, category, version, and upstream homepage";
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

      # Health check
      healthcheck.path = lib.mkOption {
        type = lib.types.str;
        default = "/";
        description = "Path for health checks (used by monitoring and homepage)";
      };

      healthcheck.url = lib.mkOption {
        type = lib.types.str;
        default = "${config.url}${config.healthcheck.path}";
        readOnly = true;
        description = "Full health check URL (derived from url and healthcheck path)";
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

      aliases = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Alternative subdomains";
      };

      # Ingress
      ingress.enable = lib.mkEnableOption "Traefik ingress route for this service" // { default = true; };

      # Access control policy (consumed by whichever auth mechanism is active)
      access.allowedGroups = lib.mkOption {
        type = lib.types.listOf (lib.types.enum (lib.attrValues cfg.groups));
        default = [ ];
        description = "Groups authorized to access this service. Empty means unrestricted (any authenticated user).";
      };

      # Ingress-level authentication (Traefik forwardAuth, mutually exclusive with OIDC)
      forwardAuth.enable = lib.mkEnableOption "ingress-level access control via Traefik forwardAuth";

      # Pre-backup hook (consumed by backup.nix)
      backup = {
        package = lib.mkOption {
          type = lib.types.nullOr lib.types.package;
          default = null;
          description = "Package providing backup script. Use writeShellApplication with runtimeInputs for dependencies.";
        };

        after = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Systemd services this backup hook requires and orders after.";
        };
      };

      # Per-service Traefik middlewares (consumed by ingress.nix)
      traefik.middlewares = lib.mkOption {
        type = lib.types.attrsOf (lib.types.attrsOf lib.types.unspecified);
        default = { };
        description = "Extra Traefik middleware definitions to attach to this service's router";
      };

    };
  };
in
{
  options.custom.homelab = {
    enable = lib.mkEnableOption "home-server services";

    # Internal extension point for per-service options (e.g., integrations).
    _serviceOptionExtensions = lib.mkOption {
      type = lib.types.listOf lib.types.deferredModule;
      default = [ ];
      internal = true;
    };

    domain = lib.mkOption {
      type = lib.types.str;
      description = "Base domain for all services (e.g. 'home.example.com')";
    };

    services = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submoduleWith {
        modules = [ baseServiceModule ] ++ cfg._serviceOptionExtensions;
      });
      default = { };
      description = ''
        Registry of homelab services: routing, metadata, and integrations.
        HTTP ingress is optional (controlled via ingress.enable).
        Integrations and security concerns extend this via internal extension hooks.
      '';
    };

    external = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ name, config, ... }: {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            default = name;
            description = "Entry identifier (defaults to attribute name)";
          };

          description = lib.mkOption {
            type = lib.types.str;
            description = "Short description";
          };

          category = lib.mkOption {
            type = categoryType;
            description = "Category for homepage grouping";
          };

          url = lib.mkOption {
            type = lib.types.str;
            description = "Direct URL to the external service";
          };

          tab = lib.mkOption {
            type = lib.types.enum [ "Home" "Admin" ];
            default = if lib.elem config.category [ "Monitoring" "Administration" ] then "Admin" else "Home";
            description = "Homepage tab to display this entry on";
          };

          icon = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = "${name}.svg";
            description = "Icon name from dashboard-icons";
          };
        };
      }));
      default = { };
      description = "External services not managed by this host (shown on homepage dashboard)";
    };

  };

  config = lib.mkIf cfg.enable {
    assertions = let
      ingressServices = lib.filter (s: s.ingress.enable) (lib.attrValues cfg.services);

      allHosts = lib.concatMap (s: [ s.publicHost ] ++ s.aliases) ingressServices;
      dupHosts = lib.filter (h: lib.count (x: x == h) allHosts > 1) (lib.unique allHosts);

      dualAuthServices = lib.filter
        (s: (lib.attrByPath [ "oidc" "enable" ] false s) && s.forwardAuth.enable)
        (lib.attrValues cfg.services);
    in [
      {
        assertion = dupHosts == [ ];
        message = "Service public hosts and aliases must be unique. Conflicting: ${toString dupHosts}";
      }
      {
        assertion = dualAuthServices == [ ];
        message = "Services must not enable both OIDC and forwardAuth. Offending: ${
          lib.concatMapStringsSep ", " (s: s.name) dualAuthServices
        }";
      }
    ];
  };
}
