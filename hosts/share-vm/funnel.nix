# Public exposure: Tailscale Funnel (TLS terminates here, so the relay sees only
# ciphertext; PROXY protocol gives Traefik the real client IP). Funnel opens at boot and
# stays up; share-manage is the operator CLI.
{ config, pkgs, lib, self, ... }:
let
  inherit (import ./lib.nix { inherit lib; }) htpasswd proxyPort publicNames darkStart darkEnd;

  # Operator CLI (fleet `*-manage` convention) and the single source of the `funnel on`
  # command — the boot service below invokes it, so manual and automatic paths can't drift:
  #   sudo share-manage rotate <scope>   # issue a password, shown once
  #   sudo share-manage funnel on|off    # manual kill switch / reopen
  shareManage = pkgs.writeShellApplication {
    name = "share-manage";
    runtimeInputs = [ pkgs.nushell pkgs.apacheHttpd pkgs.xkcdpass pkgs.coreutils pkgs.tailscale pkgs.systemd ];
    text = ''
      export HTPASSWD=${htpasswd}
      export SHARE_SCOPES=${lib.escapeShellArg publicNames}
      export FUNNEL_PORT=${toString proxyPort}
      export DARK_START=${darkStart}
      export DARK_END=${darkEnd}
      exec nu ${self.lib.builders.writeNushellScript "share-manage" ./share-manage.nu} "$@"
    '';
  };
in
{
  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets."tailscale/authkey".path;
    # Single-purpose Funnel node: no Tailscale SSH, and don't let the tailnet touch
    # the VM's DNS or routes (it keeps its own resolver and only serves Funnel).
    extraUpFlags = [ "--hostname=share-vm" "--ssh=false" "--accept-dns=false" "--accept-routes=false" ];
  };

  # Boot + the schedule timer both run `funnel auto`, which opens or closes per the dark
  # window — so a reboot inside the window comes up closed, and manual/automatic can't drift.
  systemd.services.share-funnel = {
    description = "Enforce the public Funnel endpoint's dark-window schedule";
    after = [ "tailscaled-autoconnect.service" ]; # needs the node logged in (nixpkgs ordering anchor)
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${shareManage}/bin/share-manage funnel auto";
    };
  };
  systemd.timers.share-funnel = {
    description = "Open/close the Funnel at the dark-window boundaries";
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = [ "${lib.substring 0 2 darkStart}:${lib.substring 2 2 darkStart}" "${lib.substring 0 2 darkEnd}:${lib.substring 2 2 darkEnd}" ];
  };

  environment.systemPackages = [ shareManage ];
}
