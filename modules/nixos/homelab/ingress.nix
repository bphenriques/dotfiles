{ lib, config, ... }:
let
  cfg = config.custom.homelab;
  ingressCfg = cfg.ingress;
  registry = cfg.registry;

  mkRouterConfig = service: host: {
    rule = "Host(`${host}`)";
    entryPoints = [ "websecure" ];
    service = "${service.name}-svc";
    middlewares = lib.optionals service.forwardAuth.enable [ "forwardAuth" ];
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
    };
  };
in
{
  options.custom.homelab.ingress = {
    cloudflareEmail = lib.mkOption {
      type = lib.types.str;
      description = "Cloudflare account email for DNS challenge and ACME registration";
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
            http.tls = {
              certResolver = "default";
              domains = [{
                main = cfg.domain;
                sans = [ "*.${cfg.domain}" ];
              }];
            };

            # High timeouts required for Immich large file uploads
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
          http.middlewares.forwardAuth.forwardAuth = {
            address = "${ingressCfg.forwardAuth.url}/api/auth/traefik";
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
