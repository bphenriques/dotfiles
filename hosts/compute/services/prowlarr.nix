{ config, ... }:
let
  port = 9096;
  publicUrl = config.custom.home-server.services.prowlarr.publicUrl;
in
{
  custom.home-server.services.prowlarr.internalUrl = "http://127.0.0.1:${toString port}";

  # https://wiki.servarr.com/prowlarr/environment-variables ?
  services.prowlarr = {
    enable = true;
    settings.server.port = port;
    environmentFiles = [

    ];
  };

  # PROWLARR__SERVER__URLBASE
  # PROWLARR__AUTH__APIKEY
  # Missing: download client, apps
  #

  # FIXME:
  systemd.services.prowlarr = {
    wants = [ "mnt-media.mount" ];
    after = [ "mnt-media.mount" ];
  };
}