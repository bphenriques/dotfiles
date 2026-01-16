{ config, ... }:
{
  custom.home-server.services.radarr.port = 9098;

  # https://wiki.servarr.com/radarr/environment-variables ?
  services.radarr = {
    enable = true;
    settings.server.port = config.custom.home-server.services.radarr.port;
    environmentFiles = [
      # TODO: setup radarr__AUTH__APIKEY for automations
    ];
  };

  systemd.services.radarr = {
    after = [ "mnt-nas-media.mount" ];    # Start after
    requires = [ "mnt-nas-media.mount" ]; # Stop the service if does not work.
    environment = {
      RADARR__SERVER__URLBASE = config.custom.home-server.services.radarr.publicUrl;
    };
  };
}