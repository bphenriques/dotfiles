{ config, lib, pkgs, shareVm, guestPlacement, ... }:
let
  inherit (shareVm) dataRoot proxyPort traefikMetricsPort;
  inherit (config.services.filebrowser-multiuser) authHeader;  # the header filebrowser trusts
  credsDir = "${dataRoot}/.credentials";
  htpasswd = "${credsDir}/htpasswd";
  vmIp = guestPlacement.ip;

  # Issue a one-time passphrase for a share user: 5 words (~64 bits — easy to relay,
  # uncrackable for online auth), bcrypt-hashed into the BasicAuth htpasswd and printed once.
  # In-place edit preserves the file's owner/perms; the path-watch below reloads Traefik.
  shareRotate = pkgs.writeShellApplication {
    name = "share-rotate";
    runtimeInputs = [ pkgs.apacheHttpd pkgs.xkcdpass ];
    text = ''
      users=(${lib.escapeShellArgs (lib.attrNames config.services.filebrowser-multiuser.users)})
      user="''${1:-}"
      if [[ -z "$user" ]] || ! printf '%s\n' "''${users[@]}" | grep -qxF -- "$user"; then
        echo "usage: share-rotate <user>; known: ''${users[*]}" >&2
        exit 1
      fi
      pw=$(xkcdpass -n 5 -d -)
      htpasswd -bB "${htpasswd}" "$user" "$pw"
      printf '%s\n' "$pw"
    '';
  };
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
      routers.share = {
        rule = "PathPrefix(`/`)";
        entryPoints = [ "web" ];
        service = "filebrowser";
        middlewares = [ "ratelimit" "harden" "auth" ]; # Order matters: rate-limit first before clearing headers auth
      };
      services.filebrowser.loadBalancer.servers = [{ url = "http://127.0.0.1:${toString config.services.filebrowser.settings.port}"; }];
      middlewares = {
        # Coarse per-IP DoS guard (real IP via PROXY protocol) — the random per-user
        # passphrases are the real defence, so it stays generous for photo-gallery bursts.
        ratelimit.rateLimit = { average = 30; burst = 60; };
        # Clear any client-supplied auth header before auth sets the trusted one (no header
        # injection), plus baseline security headers for the file-serving surface.
        harden.headers = {
          customRequestHeaders.${authHeader} = "";
          contentTypeNosniff = true;
          frameDeny = true;
          referrerPolicy = "no-referrer";
          stsSeconds = 31536000;
        };
        # BasicAuth is the gate; the authenticated user becomes the trusted username
        # FileBrowser reads (the Authorization header is stripped before forwarding).
        auth.basicAuth = {
          usersFile = htpasswd;
          realm = "Shared Files";
          headerField = authHeader;
          removeHeader = true;
        };
      };
    };
  };
  environment.systemPackages = [ shareRotate ];

  # htpasswd lives on the state volume (empty until `share-rotate` fills it, bcrypt).
  # localhost + bridge only: no exfiltration or LAN reach; the bridge is just for metrics.
  systemd.services.traefik = {
    after = [ "systemd-tmpfiles-setup.service" ];
    unitConfig.RequiresMountsFor = [ dataRoot ];
    serviceConfig = {
      IPAddressDeny = "any";
      IPAddressAllow = [ "localhost" guestPlacement.gateway vmIp ];
    };
  };
  systemd.tmpfiles.rules = [
    "d ${credsDir} 0750 traefik traefik -"
    "f ${htpasswd} 0640 traefik traefik -"
  ];

  # `share-rotate` just rewrites the htpasswd (proxy-agnostic); Traefik reads it only at
  # startup, so restart it when the file changes. This watch is the only Traefik-specific
  # coupling to the credential file — swap it out to use a different reverse proxy.
  systemd.paths.share-htpasswd = {
    wantedBy = [ "multi-user.target" ];
    pathConfig.PathChanged = htpasswd;
  };
  systemd.services.share-htpasswd.serviceConfig = {
    Type = "oneshot";
    ExecStart = "${pkgs.systemd}/bin/systemctl try-restart traefik.service";
  };
}
