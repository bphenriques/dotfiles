{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  baseServiceModule = { name, config, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Service identifier (defaults to attribute name)";
      };

      displayName = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Human-readable service name (defaults to attribute name)";
      };

      description = lib.mkOption {
        type = lib.types.str;
        description = "Short description of the service";
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

      healthcheck.probeModule = lib.mkOption {
        type = lib.types.enum [ "http_2xx" "http_any" ];
        default = "http_2xx";
        description = "Blackbox exporter module for health probes. Use http_any for services that require authentication on all endpoints.";
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
      ingress.enable = lib.mkEnableOption "HTTP ingress route for this service" // { default = true; };

      # Access control policy (consumed by whichever auth mechanism is active). Empty = any authenticated user.
      access.allowedGroups = lib.mkOption {
        type = lib.types.listOf (lib.types.enum (lib.attrValues cfg.groups));
        default = [ ];
        description = "Groups authorized to access this service. Empty means unrestricted (any authenticated user).";
      };

      # Pre-backup hook (consumed by backup.nix)
      backup = {
        package = lib.mkOption {
          type = lib.types.nullOr lib.types.package;
          default = null;
          description = "Package providing backup script. Use writeShellApplication with runtimeInputs for dependencies. OUTPUT_DIR is provided as an environment variable pointing to a fresh, empty directory for the hook's output.";
        };

        after = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Systemd services this backup hook requires and orders after.";
        };

      };

    };
  };
in
{
  options.custom.homelab = {
    enable = lib.mkEnableOption "home-server services";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "Base domain for all services (e.g. 'home.example.com')";
    };

    services = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submoduleWith {
        specialArgs = { homelabCfg = cfg; };
        modules = [
          baseServiceModule
          ./schemas/ingress.nix
          ./schemas/oidc.nix
          ./schemas/backup.nix
          ./schemas/resource-control.nix
          ./schemas/storage.nix
          ./schemas/homepage.nix
          ./schemas/ntfy.nix
          ./schemas/monitoring.nix
        ];
      });
      default = { };
      description = ''
        Registry of homelab services: routing, metadata, and integrations.
        HTTP ingress is optional (controlled via ingress.enable).
        Service schema is composed from base options and per-concern fragments in schemas/.
      '';
    };

    external = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule [
        ./schemas/homepage.nix
        ({ name, ... }: {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              default = name;
              description = "Entry identifier (defaults to attribute name)";
            };

            displayName = lib.mkOption {
              type = lib.types.str;
              default = name;
              description = "Human-readable entry name (defaults to attribute name)";
            };

            description = lib.mkOption {
              type = lib.types.str;
              description = "Short description";
            };

            url = lib.mkOption {
              type = lib.types.str;
              description = "Direct URL to the external service";
            };
          };

          # External entries exist solely to appear on the dashboard.
          config.integrations.homepage.enable = lib.mkDefault true;
        })
      ]);
      default = { };
      description = "External services not managed by this host (shown on homepage dashboard via integrations.homepage)";
    };

  };

  config = lib.mkIf cfg.enable {
    assertions = let
      allServices = lib.attrValues cfg.services;
      ingressServices = lib.filter (s: s.ingress.enable) allServices;

      allHosts = lib.concatMap (s: [ s.publicHost ] ++ s.aliases) ingressServices;
      dupHosts = lib.filter (h: lib.count (x: x == h) allHosts > 1) (lib.unique allHosts);

      # Services listening on the same host (default 127.0.0.1)
      allPorts = map (s: { inherit (s) name host port; }) allServices;
      dupPorts = lib.filter (p:
        lib.count (q: q.host == p.host && q.port == p.port) allPorts > 1
      ) allPorts;

      dualAuthServices = lib.filter
        (s: (lib.attrByPath [ "oidc" "enable" ] false s) && s.forwardAuth.enable)
        allServices;
    in [
      {
        assertion = dupHosts == [ ];
        message = "Service public hosts and aliases must be unique. Conflicting: ${toString dupHosts}";
      }
      {
        assertion = dupPorts == [ ];
        message = "Services have port collisions on the same host: ${
          lib.concatMapStringsSep ", " (p: "${p.name} (${p.host}:${toString p.port})") dupPorts
        }";
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
