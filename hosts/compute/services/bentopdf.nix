{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.bentopdf;
in
{
  custom.homelab.services.bentopdf = {
    displayName = "Bentopdf";
    metadata.description = "PDF Generator";
    metadata.version = pkgs.bentopdf.version;
    metadata.homepage = pkgs.bentopdf.meta.homepage;
    metadata.category = "General";
    port = 8092;
    access.allowedGroups = [ config.custom.homelab.groups.users ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
  };

  # Upstream NixOS module only serves static files via nginx/caddy.
  # We are exposing behind Traefik using darkhttpd as a lightweight static file server.
  systemd.services.bentopdf = {
    description = "Bentopdf - PDF Toolkit";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.darkhttpd}/bin/darkhttpd ${pkgs.bentopdf} --addr 127.0.0.1 --port ${toString serviceCfg.port} --no-listing --no-server-id";
      DynamicUser = true;
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictSUIDSGID = true;
      RestrictAddressFamilies = [ "AF_INET" ];
    };
  };
}
