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
      services."${service.name}-svc".loadBalancer.servers = [{ url = service.url; }];
      middlewares = service.traefik.middlewares;
    };
  };
in
{
  options.custom.homelab.ingress = {
    cloudflareEmail = lib.mkOption {
      type = lib.types.str;
      description = "Cloudflare account email for DNS challenge and ACME registration";
    };

    cloudflareTokenEnvFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to env file containing CF_DNS_API_TOKEN (must be provided by the host, e.g. via sops-nix)";
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

  config = lib.mkIf cfg.enable {
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
        EnvironmentFile = ingressCfg.cloudflareTokenEnvFile;
        Restart = "on-failure";
        RestartSec = "10s";
        RestartMaxDelaySec = "5min";
        RestartSteps = 5;
      };
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      environment.CF_API_EMAIL = ingressCfg.cloudflareEmail;
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
          email = ingressCfg.cloudflareEmail;
          storage = "/var/lib/traefik/acme.json";
          dnsChallenge.provider = "cloudflare";
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
