{ config, ... }:
{
  imports = [ ./setup.nix ];

  custom.home-server.routes.jellyseerr.port = 9099;

  services.jellyseerr = {
    enable = true;
    port = config.custom.home-server.routes.jellyseerr.port;
  };
}
