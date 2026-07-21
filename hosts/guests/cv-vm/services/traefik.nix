{ cvVm, guestPlacement, fleetFacts, ... }:
let
  inherit (cvVm) proxyPort staticPort traefikMetricsPort;
  vmIp = guestPlacement.ip;
in
{
  services.traefik = {
    enable = true;
    staticConfigOptions = {
      entryPoints = {
        # The Funnel terminates TLS and forwards here with a PROXY v2 header (real IP).
        web = {
          address = "127.0.0.1:${toString proxyPort}";
          proxyProtocol.trustedIPs = [ "127.0.0.1/32" ];
        };
        metrics.address = "${vmIp}:${toString traefikMetricsPort}";
      };
      metrics.prometheus = {
        entryPoint = "metrics";
        addServicesLabels = true;
      };
      log.level = "INFO";
    };
    dynamicConfigOptions.http = {
      routers.landing = {
        rule = "PathPrefix(`/`)";
        entryPoints = [ "web" ];
        service = "landing";
        middlewares = [ "ratelimit" "security" "signature" "notfound" ];
      };
      services.landing.loadBalancer.servers = [{ url = "http://127.0.0.1:${toString staticPort}"; }];
      middlewares = {
        # Coarse per-IP DoS guard (real IP via PROXY protocol): 5 req/s, small burst.
        ratelimit.rateLimit = { average = 5; burst = 10; };
        # Baseline hardening for the public surface.
        security.headers = {
          contentTypeNosniff = true;
          frameDeny = true;
          referrerPolicy = "no-referrer";
          stsSeconds = 31536000;
        };
        # Easter-egg headers only a curl user notices.
        signature.headers.customResponseHeaders = {
          "X-Declared-In" = "github.com/bphenriques/dotfiles";
          "X-Fleet" = "${toString fleetFacts.hosts} hosts · ${toString fleetFacts.services} services";
          "X-Sla" = "best effort";
          "X-See-Also" = "/humans.txt";
        };
        notfound.errors = {
          status = [ "404" ];
          service = "landing";
          query = "/404.html";
        };
      };
    };
  };

  # localhost + bridge only: no exfiltration or LAN reach; the bridge is just for metrics.
  systemd.services.traefik.serviceConfig = {
    IPAddressDeny = "any";
    IPAddressAllow = [ "localhost" guestPlacement.gateway vmIp ];
  };
}
