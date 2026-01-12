{ config, ... }:
{
  custom.home-server.services.prowlarr.port = 9096;

  # https://wiki.servarr.com/prowlarr/environment-variables ?
  services.prowlarr = {
    enable = true;
    settings.server.port = config.custom.home-server.services.prowlarr.port;
    environmentFiles = [
      # TODO: setup PROWLARR__AUTH__APIKEY for automations
    ];
  };

  systemd.services.prowlarr = {
    after = [ "mnt-nas-media.mount" ];    # Start after
    requires = [ "mnt-nas-media.mount" ]; # Stop the service if does not work.
    environment = {
      PROWLARR__SERVER__URLBASE = config.custom.home-server.services.prowlarr.publicUrl;
    };
  };
}