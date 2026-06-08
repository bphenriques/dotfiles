{ lib, config, ... }:
let
  cfg = config.custom.homelab;
  ingressCfg = cfg.ingress;

  mkRouterConfig = service: host: {
    rule = "Host(`${host}`)";
    entryPoints = [ "websecure" ];
    service = "${service.name}-svc";
    middlewares = lib.optionals service.forwardAuth.enable [ "forwardAuth" ]
      ++ lib.attrNames service.traefik.middlewares;
  };

  mkTraefikRoute = service: let
    aliasRouters = lib.imap0 (i: alias: {
      name = "${service.name}-alias-${toString i}";
      value = mkRouterConfig service alias;
    }) service.aliases;
  in {
    http = {
      routers = {
        "${service.name}" = mkRouterConfig service service.publicHost;
      } // lib.listToAttrs aliasRouters;
      services."${service.name}-svc".loadBalancer.servers = [{ inherit (service) url; }];
      inherit (service.traefik) middlewares;
    };
  };
in
{
  options.custom.homelab.ingress = {
    traefik.enable = lib.mkEnableOption "Traefik reverse-proxy ingress implementation";

    acme = {
      email = lib.mkOption {
        type = lib.types.str;
        description = "ACME account email for certificate registration";
      };

      dnsProvider = lib.mkOption {
        type = lib.types.str;
        description = "lego DNS-01 challenge provider name (e.g. 'cloudflare'); see the Traefik/lego provider list";
      };

      credentialsEnvFile = lib.mkOption {
        type = lib.types.str;
        description = "Path to an env file with the DNS provider's credentials (e.g. CF_DNS_API_TOKEN). Provided by the host, e.g. via sops-nix.";
      };
    };

    metricsPort = lib.mkOption {
      type = lib.types.port;
      default = 8082;
      description = "Port for Traefik's Prometheus metrics endpoint (localhost only)";
    };

    allowedInterfaces = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Network interfaces to allow HTTP/HTTPS traffic on. If empty, allows on all interfaces (not recommended).";
    };

    forwardAuth = {
      enable = lib.mkEnableOption "forwardAuth middleware";

      url = lib.mkOption {
        type = lib.types.str;
        description = "URL of the forward auth service";
      };
    };
  };

  config = lib.mkIf cfg.ingress.traefik.enable {
    custom.homelab.monitoring.scopes.traefik = {
      scrapeConfigs = [{
        job_name = "traefik";
        scrape_interval = "120s";
        static_configs = [{
          targets = [ "127.0.0.1:${toString ingressCfg.metricsPort}" ];
          labels.instance = config.networking.hostName;
        }];
      }];
    };

    assertions = let
      forwardAuthServices = lib.filter (s: s.forwardAuth.enable && s.ingress.enable) (lib.attrValues cfg.services);
    in [
      {
        assertion = forwardAuthServices == [ ] || ingressCfg.forwardAuth.enable;
        message = "Services have forwardAuth enabled but custom.homelab.ingress.forwardAuth.enable is false: ${
          lib.concatMapStringsSep ", " (s: s.name) forwardAuthServices
        }. Traefik would silently skip auth for these services.";
      }
    ];

    networking.firewall = if ingressCfg.allowedInterfaces == []
      then { allowedTCPPorts = [ 80 443 ]; }
      else { interfaces = lib.genAttrs ingressCfg.allowedInterfaces (_: { allowedTCPPorts = [ 80 443 ]; }); };

    systemd.services.traefik = {
      serviceConfig = {
        EnvironmentFile = ingressCfg.acme.credentialsEnvFile;
        Restart = "on-failure";
        RestartSec = "10s";
        RestartMaxDelaySec = "5min";
        RestartSteps = 5;
      };
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
    };

    # No rate limiting: all services are behind WireGuard/LAN, keeping config simple.
    services.traefik = {
      enable = true;
      staticConfigOptions = {
        log.level = "ERROR";
        entryPoints = {
          web = {
            address = "0.0.0.0:80";
            http.redirections.entryPoint = {
              to = "websecure";
              scheme = "https";
            };
          };

          websecure = {
            address = "0.0.0.0:443";

            # Extended timeouts for large uploads (Immich). Entrypoint-scoped (Traefik has no per-router timeout).
            transport.respondingTimeouts = {
              readTimeout = "600s";
              idleTimeout = "600s";
            };
            http.tls = {
              certResolver = "default";
              domains = [{
                main = cfg.domain;
                sans = [ "*.${cfg.domain}" ];
              }];
            };
          };
        };

        entryPoints.metrics.address = "127.0.0.1:${toString ingressCfg.metricsPort}";
        metrics.prometheus = {
          entryPoint = "metrics";
          addRoutersLabels = true;
          addServicesLabels = true;
        };

        certificatesResolvers.default.acme = {
          email = ingressCfg.acme.email;
          storage = "/var/lib/traefik/acme.json";
          dnsChallenge.provider = ingressCfg.acme.dnsProvider;
        };
      };

      dynamicConfigOptions = lib.pipe (lib.attrValues cfg.services) [
        (lib.filter (s: s.ingress.enable))
        (map mkTraefikRoute)
        (routes: routes ++ lib.optional ingressCfg.forwardAuth.enable {
          http.middlewares.forwardAuth.forwardAuth = {
            address = "${ingressCfg.forwardAuth.url}/api/auth/traefik";
            trustForwardHeader = true;
            authResponseHeaders = [
              "Remote-User"
              "Remote-Email"
              "Remote-Groups"
              "Remote-Name"
            ];
          };
        })
        (lib.foldl' lib.recursiveUpdate { })
      ];
    };
  };
}
