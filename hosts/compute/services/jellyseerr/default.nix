{ config, ... }:
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.jellyseerr.port = 9099;

  sops.secrets."jellyseerr/api-key" = { };
  sops.templates."jellyseerr.env".content = ''
    API_KEY=${config.sops.placeholder."jellyseerr/api-key"}
  '';

  services.jellyseerr = {
    enable = true;
    port = config.custom.home-server.routes.jellyseerr.port;
  };

  systemd.services.jellyseerr.serviceConfig.EnvironmentFile = config.sops.templates."jellyseerr.env".path;
}
