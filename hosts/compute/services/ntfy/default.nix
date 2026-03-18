{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.ntfy;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.ntfy = {
    description = "Push Notifications";
    version = config.services.ntfy-sh.package.version;
    homepage = config.services.ntfy-sh.package.meta.homepage;
    category = "Monitoring";
    port = 2586;
    healthcheck.path = "/v1/health";
    forwardAuth = {
      enable = true;
      groups = with config.custom.homelab.groups; [ users admin ];
    };
    integrations.homepage.enable = true;
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
