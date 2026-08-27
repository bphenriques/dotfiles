{ config, ... }:
{
  selfhost.apps.sonarr = {
    enable = true;
    configureAfter = [ "transmission.service" ]; # sonarr does connection tests when configuring
    rootFolders = [{
      path = "${config.custom.shares.media.root}/tv";
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
        tvCategory = config.custom.fleet.media.downloadCategories.sonarr;
      };
    }];
    notifyOnImport = false; # Seerr announces arrivals to the family; this connection stays operational
    delayProfile = {
      preferredProtocol = "torrent";
      torrentDelay = 120;
    };
  };

  selfhost.services.sonarr = {
    meta.category = "media automation";
    storage.mounts = [ "media" ];
    integrations.notify.topic = "admin"; # health/manual-interaction flags: ops signal, not family-facing
    integrations.homepage.group = "Admin";
    extraConfig.landingPage.enable = true;
  };

  users.users.sonarr.extraGroups = [ config.selfhost.storage.mounts.smb.shares.media.group ];

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
