{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.ntfy;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.ntfy = {
    displayName = "Ntfy";
    metadata.description = "Push Notifications";
    metadata.version = config.services.ntfy-sh.package.version;
    metadata.homepage = config.services.ntfy-sh.package.meta.homepage;
    metadata.category = "Monitoring";
    port = 2586;
    healthcheck.path = "/v1/health";
    integrations.homepage.enable = true;
    integrations.homepage.tab = "Admin";
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
