# Public reverse proxy (Traefik): per-user BasicAuth → Remote-User, a rate limit, and the
# PROXY-protocol web entrypoint the Funnel terminates onto. Native Prometheus metrics are
# served on the bridge so the dashboard can show responses by status. Binds localhost (the
# Funnel + FileBrowser) and the bridge IP (metrics) only — nothing else.
{ lib, ... }:
let
  inherit (import ../../shared.nix) microvm;
  inherit (import ../lib.nix { inherit lib; }) dataRoot credsDir htpasswd fbPort proxyPort;
  vmIp = microvm.hosts.share-vm;
  metricsPort = 9117;
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
        metrics.address = "${vmIp}:${toString metricsPort}";
      };
      metrics.prometheus = {
        entryPoint = "metrics";
        addServicesLabels = true;
      };
      log.level = "INFO";
      # No API/dashboard.
    };
    dynamicConfigOptions.http = {
      routers.share = {
        rule = "PathPrefix(`/`)";
        entryPoints = [ "web" ];
        service = "filebrowser";
        # Order matters: rate-limit first (so it also covers pre-auth attempts), strip any
        # client Remote-User, then BasicAuth sets the trusted one.
        middlewares = [ "ratelimit" "harden" "auth" ];
      };
      services.filebrowser.loadBalancer.servers = [{ url = "http://127.0.0.1:${toString fbPort}"; }];
      middlewares = {
        # Coarse per-IP DoS guard (real IP via PROXY protocol) — the random per-user
        # passphrases are the real defence, so it stays generous for photo-gallery bursts.
        ratelimit.rateLimit = { average = 30; burst = 60; };
        # Clear any client-supplied Remote-User before auth sets the trusted one (no header
        # injection), plus baseline security headers for the file-serving surface.
        harden.headers = {
          customRequestHeaders.Remote-User = "";
          contentTypeNosniff = true;
          frameDeny = true;
          referrerPolicy = "no-referrer";
          stsSeconds = 31536000;
        };
        # BasicAuth is the gate; the authenticated user becomes the trusted Remote-User
        # FileBrowser reads (the Authorization header is stripped before forwarding).
        auth.basicAuth = {
          usersFile = htpasswd;
          realm = "Shared Files";
          headerField = "Remote-User";
          removeHeader = true;
        };
      };
    };
  };
  # htpasswd lives on the state volume (created empty below); `share-manage rotate` fills
  # it (bcrypt). localhost + bridge only: a compromise can't exfiltrate or reach the LAN
  # (the bridge IP/gateway are needed for the metrics scrape, nothing more).
  systemd.services.traefik = {
    after = [ "systemd-tmpfiles-setup.service" ];
    unitConfig.RequiresMountsFor = [ dataRoot ];
    serviceConfig = {
      IPAddressDeny = "any";
      IPAddressAllow = [ "localhost" microvm.bridge.gateway vmIp ];
    };
  };
  systemd.tmpfiles.rules = [
    "d ${credsDir} 0750 traefik traefik -"
    "f ${htpasswd} 0640 traefik traefik -"
  ];
}
