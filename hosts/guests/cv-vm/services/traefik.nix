{ cvVm, guestPlacement, fleetFacts, ... }:
let
  inherit (cvVm) tunnelPort staticPort traefikMetricsPort;
  vmIp = guestPlacement.ip;
in
{
  services.traefik = {
    enable = true;
    staticConfigOptions = {
      entryPoints = {
        tunnel.address = "127.0.0.1:${toString tunnelPort}"; # cloudflared forwards here. The real client IP arrives in CF-Connecting-IP.
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
        entryPoints = [ "tunnel" ];
        service = "landing";
        middlewares = [ "ratelimit" "security" "signature" "notfound" ];
      };
      services.landing.loadBalancer.servers = [{ url = "http://127.0.0.1:${toString staticPort}"; }];
      middlewares = {
        ratelimit.rateLimit = {
          average = 5;
          burst = 10;
          sourceCriterion.requestHeaderName = "CF-Connecting-IP"; # The IP as sent by Cloudflare
        };
        security.headers = {
          contentTypeNosniff = true;
          frameDeny = true;
          referrerPolicy = "no-referrer";
          stsSeconds = 31536000;
        };
        # Small easter-egg for those who call with curl
        signature.headers.customResponseHeaders = {
          "X-Declared-In" = "github.com/bphenriques/dotfiles";
          "X-Fleet" = "${toString fleetFacts.hosts} hosts · ${toString (builtins.length fleetFacts.services)} services";
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

  # localhost + bridge (metrics) only, no LAN reach.
  systemd.services.traefik.serviceConfig = {
    IPAddressDeny = "any";
    IPAddressAllow = [ "localhost" guestPlacement.gateway vmIp ];
  };
}
