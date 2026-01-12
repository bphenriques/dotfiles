{ config, ... }:
{
  custom.home-server.services.jellyseer.port = 9099;
  services.jellyseerr = {
    enable = true;
    port = config.custom.home-server.services.jellyseer.port;
  };

  # TODO: consider API_KEY to automate some tasks
}