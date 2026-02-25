# Traefik Ingress Module
#
# Consumes the service registry and configures Traefik routing.
# Handles TLS certificates, HTTP->HTTPS redirection, and forwardAuth middleware.
#
{ lib, config, ... }:
let
  cfg = config.custom.homelab;
  ingressCfg = cfg.ingress.traefik;
  registry = cfg._registry;

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
in
{
  options.custom.homelab.ingress.traefik = {
    cloudflareEmail = lib.mkOption {
      type = lib.types.str;
      description = "Cloudflare account email for DNS challenge and ACME registration";
    };

    forwardAuth = {
      enable = lib.mkEnableOption "forwardAuth middleware via tinyauth";

      internalUrl = lib.mkOption {
        type = lib.types.str;
        description = "Internal URL of the forward auth service (e.g. tinyauth)";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall = {
      allowedTCPPorts = [ 80 443 ];
      allowedUDPPorts = [ 443 ];
    };

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
      serviceConfig.EnvironmentFile = config.sops.templates."traefik-cloudflare".path;
      environment.CF_API_EMAIL = ingressCfg.cloudflareEmail;
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
          email = ingressCfg.cloudflareEmail;
          storage = "/var/lib/traefik/acme.json";
          dnsChallenge.provider = "cloudflare";
        };
      };

      dynamicConfigOptions = let
        routeConfigs = map mkTraefikRoute registry.allServices;
        routesConfig = lib.foldl' lib.recursiveUpdate { } routeConfigs;
        authMiddleware = lib.optionalAttrs ingressCfg.forwardAuth.enable {
          http.middlewares.tinyauth.forwardAuth = {
            address = "${ingressCfg.forwardAuth.internalUrl}/api/auth/traefik";
            trustForwardHeader = true;
            authResponseHeaders = [
              "X-Forwarded-User"
              "X-Forwarded-Groups"
              "X-Forwarded-Email"
            ];
          };
        };
      in lib.foldl' lib.recursiveUpdate { } [ routesConfig authMiddleware ];
    };
  };
}
