{ config, ... }:
{
  selfhost.apps.sonarr = {
    enable = true;
    configureAfter = [ "transmission.service" ]; # sonarr does connection tests when configuring
    rootFolders = [{
      path = config.custom.paths.media.tv;
      defaultQualityProfile = config.custom.media.sonarr.profiles.default.name;
    }];
    downloadClients = [{
      name = "Transmission";
      implementation = "Transmission";
      protocol = "torrent";
      fields = {
        host = "127.0.0.1";
        inherit (config.selfhost.services.transmission) port;
        urlBase = "/transmission/";
        tvCategory = "sonarr";
      };
    }];
    delayProfile = {
      preferredProtocol = "torrent";
      torrentDelay = 120;
    };
  };

  selfhost.services.sonarr = {
    storage.smb = [ "media" ];
    integrations.notify.topic = "media";
    integrations.homepage.group = "Admin";
    extraConfig.landingPage.enable = true;
  };

  users.users.sonarr.extraGroups = [ config.selfhost.storage.smb.mounts.media.group ];

  # Quality taste (recyclarr / TRaSH guides) — consumer-owned; never in the framework.
  custom.media.sonarr = {
    qualityDefinitionType = "series";
    profiles = {
      default = {
        name = "WEB-1080p";
        trashId = "72dae194fc92bf828f32cde7744e51a1";
      };
      uhd = {
        name = "WEB-2160p";
        trashId = "d1498e7d189fbe6c7110ceaabb7473e6";
      };
    };
  };
}
