{ config, ... }:
{
  selfhost.apps.radarr = {
    enable = true;
    configureAfter = [ "transmission.service" ];  # radarr does connection tests when configuring
    rootFolders = [{
      path = "${config.fleet.shares.media.root}/movies";
      defaultQualityProfile = config.my.media.radarr.profiles.default.name;
    }];
    downloadClients = [
      {
        name = "Transmission";
        implementation = "Transmission";
        protocol = "torrent";
        fields = {
          host = "127.0.0.1";
          inherit (config.selfhost.services.transmission) port;
          urlBase = "/transmission/";
          movieCategory = config.fleet.media.downloadCategories.radarr;
        };
      }
    ];
    notifyOnImport = false;
    delayProfile = {
      preferredProtocol = "torrent";
      torrentDelay = 120;
    };
  };

  selfhost.services.radarr = {
    meta.category = "media automation";
    storage.mounts = [ "media" ];
    storage.users = [ "radarr" ];
    integrations.notify.topic = "admin";
    integrations.homepage.group = "Admin";
    extraConfig.landingPage.enable = true;
  };

  # Quality taste (recyclarr / TRaSH guides): consumer-owned, never in the framework.
  my.media.radarr = {
    qualityDefinitionType = "movie";
    profiles = {
      default = {
        name = "HD Bluray + WEB";
        trashId = "d1d67249d3890e49bc12e275d989a7e9";
      };
      uhd = {
        name = "UHD Bluray + WEB";
        trashId = "64fb5f9858489bdac2af690e27c8f42f";
      };
    };
  };
}
