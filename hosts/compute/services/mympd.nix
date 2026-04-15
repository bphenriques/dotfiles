{ config, lib, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.mympd;
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
      MPD_HOST = self.shared.networks.main.hosts.inky;
      MPD_PORT = toString 6600;
    };
    path = [ pkgs.coreutils ];
    preStart = lib.mkAfter ''
      pin_hash="$(tr -d '\n' < ${lib.escapeShellArg config.sops.secrets."mympd/pin".path} | sha256sum | cut -d' ' -f1)"
      printf '%s' "$pin_hash" > "$STATE_DIRECTORY/config/pin_hash"
      chmod 0600 "$STATE_DIRECTORY/config/pin_hash"
    '';
  };

  custom.homelab.services.mympd = {
    displayName = "My MPD";
    metadata.description = "Remote MPD Client";
    metadata.version = pkgs.mympd.version;
    metadata.homepage = pkgs.mympd.meta.homepage;
    metadata.category = "General";
    port = 8093;
    access.allowedGroups = [ config.custom.homelab.groups.users ];
    forwardAuth.enable = false; # Not required as settings are protected and the service only has read-only permissions
    integrations.homepage.enable = true;
  };
}