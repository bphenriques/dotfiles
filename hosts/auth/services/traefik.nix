{ config, self, ... }:
let
  fqdn = "auth.${self.settings.compute.domain}";
  cloudflareEmail = self.settings.cloudflareEmail;
in
{
  sops = {
    secrets.cloudflare_dns_api_token = { };
    templates."traefik-external-env" = {
      owner = "traefik";
      content = ''CF_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_dns_api_token}'';
    };
  };

  systemd.services.traefik = {
    serviceConfig.EnvironmentFile = config.sops.templates."traefik-external-env".path;
    environment.CF_API_EMAIL = cloudflareEmail;
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
            certResolver = "external";
            domains = [{ main = fqdn; }];
          };

          transport.respondingTimeouts = {
            readTimeout = "60s";
            writeTimeout = "60s";
          };
        };
      };

      certificatesResolvers.external.acme = {
        email = cloudflareEmail;
        storage = "/var/lib/traefik/acme.json";
        dnsChallenge.provider = "cloudflare";
      };
    };

    dynamicConfigOptions = {
      http = {
        middlewares.rate-limit.rateLimit = {
          average = 50;
          burst = 100;
        };

        routers.main = {
          rule = "Host(`${fqdn}`)";
          entryPoints = [ "websecure" ];
          service = "main-svc";
          middlewares = [ "rate-limit" ];
        };

        services."main-svc".loadBalancer.servers = [{ url = "http://127.0.0.1:8082"; }];
      };
    };
  };
}