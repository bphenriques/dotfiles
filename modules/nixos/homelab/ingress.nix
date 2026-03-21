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

    metricsPort = lib.mkOption {
      type = lib.types.port;
      default = 8082;
      description = "Port for Traefik's Prometheus metrics endpoint (localhost only)";
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

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    sops = {
      secrets.cloudflare_dns_api_token = { };
      templates."traefik-cloudflare" = {
        owner = "traefik";
        content = ''
          CF_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_dns_api_token}
        '';
      };
    };

    systemd.services.traefik = {
      serviceConfig = {
        EnvironmentFile = config.sops.templates."traefik-cloudflare".path;
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

            # Immich requires longer timeouts for large photo/video uploads; without this, Traefik's
            # default 60s readTimeout causes uploads to fail with HTTP 499 after 1 minute.
            # Ref: https://docs.immich.app/administration/reverse-proxy (Traefik section)
            # Note: this is entrypoint-scoped (Traefik has no per-router timeout), so it affects all
            # services. Acceptable in a homelab with limited concurrent users.
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

      dynamicConfigOptions = let
        routeConfigs = map mkTraefikRoute (lib.filter (s: s.ingress.enable) (lib.attrValues cfg.services));
        routesConfig = lib.foldl' lib.recursiveUpdate { } routeConfigs;
        authMiddleware = lib.optionalAttrs ingressCfg.forwardAuth.enable {
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
        };
      in lib.foldl' lib.recursiveUpdate { } [ routesConfig authMiddleware ];
    };
  };
}
