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

      # Service metadata
      category = lib.mkOption {
        type = categoryType;
        description = "Service category used by generated catalogue/homepage views";
      };

      description = lib.mkOption {
        type = lib.types.str;
        description = "Short description of the service";
      };

      version = lib.mkOption {
        type = lib.types.str;
        description = "Service version (e.g. '4.7.0')";
      };

      homepage = lib.mkOption {
        type = lib.types.str;
        description = "Upstream project URL";
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

      aliases = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Alternative subdomains";
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

      # Pre-backup hook (consumed by backup.nix)
      backup = {
        script = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "Script to run before backup. Should write output to the directory specified by OUTPUT_DIR env var.";
        };

        environment = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
          description = "Environment variables passed to the backup script.";
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
        Registry of HTTP services: routing and shared defaults.
        Integrations and security concerns extend this via internal extension hooks.
        Each service gets a subdomain under the base domain.
      '';
    };

  };

  config = lib.mkIf cfg.enable {
    assertions = let
      allServices = lib.attrValues cfg.services;

      urls = map (s: s.url) allServices;
      dupUrls = lib.filter (url: lib.count (u: u == url) urls > 1) (lib.unique urls);

      allHosts = lib.concatMap (s: [ s.publicHost ] ++ s.aliases) allServices;
      dupHosts = lib.filter (h: lib.count (x: x == h) allHosts > 1) (lib.unique allHosts);
    in [
      {
        assertion = dupUrls == [ ];
        message = "Service URLs must be unique. Conflicting: ${toString dupUrls}";
      }
      {
        assertion = dupHosts == [ ];
        message = "Service public hosts and aliases must be unique. Conflicting: ${toString dupHosts}";
      }
    ];
  };
}
