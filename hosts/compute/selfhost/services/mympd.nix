{ config, lib, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.mympd;
in
{
  selfhost.services.mympd = {
    displayName = "My MPD";
    meta.homepage = "https://jcorporation.github.io/myMPD/";
    meta.description = "Remote MPD Client";
    meta.category = "media";
    port = 8093;
    # Read-only remote control with its settings locked down, so it is served unauthenticated rather
    # than gated. Nothing enforces groups here, so none are named.
    access.model = "open";
    extraConfig.landingPage.enable = true;
  };

  services.mympd = {
    enable = true;
    settings = {
      acl = "+127.0.0.0/8";
      http_host = "127.0.0.1";
      http_port = serviceCfg.port;
    };
  };

  sops.secrets."mympd/pin" = { };
  systemd.services.mympd = {
    environment = {
      MPD_HOST = config.custom.fleet.lan.hosts.inky;
      MPD_PORT = toString 6600;
    };
    path = [ pkgs.coreutils ];
    preStart = lib.mkAfter ''
      pin_hash="$(tr -d '\n' < ${lib.escapeShellArg config.sops.secrets."mympd/pin".path} | sha256sum | cut -d' ' -f1)"
      printf '%s' "$pin_hash" > "$STATE_DIRECTORY/config/pin_hash"
      chmod 0600 "$STATE_DIRECTORY/config/pin_hash"
    '';
  };
}