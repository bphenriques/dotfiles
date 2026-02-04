{ config, ... }:
let
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.sonarr = {
    port = 9097;
    requiresAuth = true;
  };

  sops.secrets."sonarr/api-key" = { };
  sops.templates."sonarr.env".content = ''
    SONARR__AUTH__APIKEY=${config.sops.placeholder."sonarr/api-key"}
  '';

  services.sonarr = {
    enable = true;
    settings.server.port = config.custom.home-server.routes.sonarr.port;
    environmentFiles = [ config.sops.templates."sonarr.env".path ];
  };

  systemd.services.sonarr.environment = {
    SONARR__AUTH__METHOD = "External";
    SONARR__LOG__LEVEL = "info";
  };

  users.users.sonarr.extraGroups = [ homelabMounts.media.group ];
  systemd.services.sonarr = {
    requires = [ homelabMounts.media.automountUnit ];
    after = [ homelabMounts.media.automountUnit ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };
}
