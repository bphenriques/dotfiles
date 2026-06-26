{ config, pkgs, lib, shareVm, ... }:
let
  inherit (shareVm) proxyPort;
in
{
  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets."tailscale/authkey".path;
    # Single-purpose Funnel node: no Tailscale SSH, and don't let the tailnet touch the VM's
    # DNS or routes (it keeps its own resolver and only serves Funnel).
    extraUpFlags = [ "--hostname=share-vm" "--ssh=false" "--accept-dns=false" "--accept-routes=false" ];
  };

  # Establish the Funnel serve config at boot (idempotent; nixpkgs has no declarative serve
  # option). It persists in tailscale state, so the kill-switch is just `tailscale down`/`up`
  # — no wrapper needed (see README).
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
