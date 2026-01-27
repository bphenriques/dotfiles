{ config, ... }:
{
  custom.home-server.routes.jellyseer.port = 9099;
  services.jellyseerr = {
    enable = true;
    port = config.custom.home-server.routes.jellyseer.port;
  };

  # TODO: consider API_KEY to automate some tasks:
  # - Register sonarr
  # - Register radarr
  # - Basic settings
}