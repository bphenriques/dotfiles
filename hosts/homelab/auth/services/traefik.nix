{ config, lib, self, ... }:
let
  networking = import ../../networking.nix;
  fqdn = "auth.${self.settings.compute.domain}";
  cloudflareEmail = self.settings.cloudflareEmail;
  pocketIdInternalUrl = "http://127.0.0.1:8082";
  internalSubnet = networking.bridge.subnet;
  cloudflareIPs = networking.cloudflare.ipv4 ++ networking.cloudflare.ipv6;

  # Public API endpoints. Not permissive by design. Not even login-code for now.
  # TODO: Review Pocket-ID docs/source to ensure this list is complete for:
  #       - Passkey registration flow
  #       - Invite token validation
  #       - Any other user-facing API needed externally
  publicApiPaths = [
    "/api/webauthn"               # Passkey registration/authentication
    "/api/users/profile-picture"  # Setting profile picture
  ];
  publicApiRule = lib.concatStringsSep " || " (map (p: "PathPrefix(`${p}`)") publicApiPaths);
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

          forwardedHeaders.trustedIPs = cloudflareIPs; # Trust Cloudflare to provide real client IP via X-Forwarded-For
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
        middlewares = {
          # Rate limiting for external traffic (uses real client IP via XFF)
          rate-limit.rateLimit = {
            average = 50;
            burst = 100;
          };

          # Internal allowlist for admin API (defense in depth)
          internal-only.ipAllowList.sourceRange = [ internalSubnet ];
        };

        routers = {
          # Internal admin API: only from bridge network
          internal-api = {
            rule = "Host(`${fqdn}`) && PathPrefix(`/api`) && ClientIP(`${internalSubnet}`)";
            entryPoints = [ "websecure" ];
            service = "pocketid-svc";
            middlewares = [ "internal-only" ];
            priority = 100;
            tls = { };
          };

          # Public: everything except /api, plus allowed public API paths (registration flow)
          public = {
            rule = "Host(`${fqdn}`) && (!PathPrefix(`/api`) || ${publicApiRule})";
            entryPoints = [ "websecure" ];
            service = "pocketid-svc";
            middlewares = [ "rate-limit" ];
            priority = 10;
            tls = { };
          };
        };

        services."pocketid-svc".loadBalancer.servers = [{ url = pocketIdInternalUrl; }];
      };
    };
  };
}
