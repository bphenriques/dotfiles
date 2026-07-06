{ config, pkgs, lib, shareVm, ... }:
let
  inherit (shareVm) proxyPort;
in
{
  sops.secrets."tailscale/authkey" = { };
  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets."tailscale/authkey".path;
    # Single-purpose Funnel node: no Tailscale SSH, and don't let the tailnet touch the VM's
    # DNS or routes (it keeps its own resolver and only serves Funnel).
    extraUpFlags = [ "--hostname=share-vm" "--ssh=false" "--accept-dns=false" "--accept-routes=false" ];
  };

  # Open funnel at boot
  systemd.services.share-funnel = {
    description = "Open the public Funnel endpoint";
    after = [ "tailscaled-autoconnect.service" ]; # needs the node logged in (nixpkgs ordering anchor)
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${lib.getExe' pkgs.tailscale "tailscale"} funnel --bg --proxy-protocol=2 --tls-terminated-tcp=443 tcp://127.0.0.1:${toString proxyPort}";
    };
  };
}
