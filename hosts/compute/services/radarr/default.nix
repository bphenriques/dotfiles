{ config, ... }:
let
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.radarr = {
    port = 9098;
    requiresAuth = true;
  };

  sops.secrets."radarr/api-key" = { };
  sops.templates."radarr.env".content = ''
    RADARR__AUTH__APIKEY=${config.sops.placeholder."radarr/api-key"}
  '';

  services.radarr = {
    enable = true;
    settings.server.port = config.custom.home-server.routes.radarr.port;
    environmentFiles = [ config.sops.templates."radarr.env".path ];
  };

  systemd.services.radarr.environment.RADARR__AUTH__METHOD = "External";

  users.users.radarr.extraGroups = [ homelabMounts.media.group ];
  systemd.services.radarr = {
    requires = [ homelabMounts.media.automountUnit ];
    after = [ homelabMounts.media.automountUnit ];
  };
}
