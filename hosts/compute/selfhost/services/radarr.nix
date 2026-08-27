{ config, ... }:
{
  selfhost.apps.radarr = {
    enable = true;
    configureAfter = [ "transmission.service" ];  # radarr does connection tests when configuring
    rootFolders = [{
      path = "${config.custom.shares.media.root}/movies";
      defaultQualityProfile = config.custom.media.radarr.profiles.default.name;
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
          movieCategory = config.custom.fleet.media.downloadCategories.radarr;
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
    integrations.notify.topic = "admin";
    integrations.homepage.group = "Admin";
    extraConfig.landingPage.enable = true;
  };

  users.users.radarr.extraGroups = [ config.selfhost.storage.mounts.smb.shares.media.group ];

  # Quality taste (recyclarr / TRaSH guides) — consumer-owned; never in the framework.
  custom.media.radarr = {
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
