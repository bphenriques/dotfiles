{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.ntfy;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.ntfy = {
    port = 2586;
    integrations.homepage = {
      enable = true;
      category = "Admin";
      description = "Push Notifications";
    };
  };

  services.ntfy-sh = {
    enable = true;
    settings = {
      base-url = serviceCfg.publicUrl;
      listen-http = "${serviceCfg.host}:${toString serviceCfg.port}";
      behind-proxy = true;
      auth-default-access = "deny-all";
      enable-login = true;
    };
  };

  systemd.services.ntfy-sh.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
  };
}
