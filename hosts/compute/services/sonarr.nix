{ config, ... }:
{
  custom.home-server.services.sonarr.port = 9097;

  # https://wiki.servarr.com/prowlarr/environment-variables ?
  services.sonarr = {
    enable = true;
    settings.server.port = config.custom.home-server.services.sonarr.port;
    environmentFiles = [
      # TODO: setup SONARR__AUTH__APIKEY for automations
    ];
  };

  systemd.services.sonarr = {
    after = [ "mnt-nas-media.mount" ];    # Start after
    requires = [ "mnt-nas-media.mount" ]; # Stop the service if does not work.
    environment = {
      SONARR__SERVER__URLBASE = config.custom.home-server.services.sonarr.publicUrl;
    };
  };
}