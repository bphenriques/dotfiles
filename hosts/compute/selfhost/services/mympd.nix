{ config, lib, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.mympd;
in
{
  sops.secrets."mympd/pin" = { };

  services.mympd = {
    enable = true;
    settings = {
      acl = "+127.0.0.0/8";
      http_host = "127.0.0.1";
      http_port = serviceCfg.port;
    };
  };

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

  selfhost.services.mympd = {
    displayName = "My MPD";
    description = "Remote MPD Client";
    port = 8093;
    access.allowedGroups = [ config.selfhost.groups.users ];
    forwardAuth.enable = false; # Not required as settings are protected and the service only has read-only permissions
  };
}