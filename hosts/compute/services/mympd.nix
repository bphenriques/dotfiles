{ config, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.mympd;
in
{
  services.mympd = {
    enable = true;
    settings = {
      acl = "+127.0.0.0/8";
      http_host = "127.0.0.1";
      http_port = serviceCfg.port;
    };
  };

  systemd.services.mympd.environment = {
    MPD_HOST = self.shared.networks.main.hosts.inky;
    MPD_PORT = toString 6600;
  };

  custom.homelab.services.mympd = {
    displayName = "My MPD";
    metadata.description = "Remote MPD Client";
    metadata.version = pkgs.mympd.version;
    metadata.homepage = pkgs.mympd.meta.homepage;
    metadata.category = "General";
    port = 8093;
    access.allowedGroups = [ config.custom.homelab.groups.users ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
  };
}