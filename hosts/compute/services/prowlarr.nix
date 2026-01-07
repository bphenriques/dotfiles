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
    settings = {
      update.mechanism = "internal";
      server = {
        inherit port;
        urlbase = publicUrl;
        bindaddress = "127.0.0.1";
      };
    };
  };
}