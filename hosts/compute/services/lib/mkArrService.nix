# Creates a standardized *arr service (radarr/sonarr) module
#
# Assumptions:
# - Transmission is the only download client used
# - config.custom.homelab.services.transmission.port is defined
# - config.custom.homelab.media.${name}.profiles.default exists
#
# Usage (in imports):
#   (import ./lib/mkArrService.nix {
#     name = "radarr";
#     port = 9098;
#     description = "Movie Tracker";
#     rootPath = config: config.custom.homelab.paths.media.movies;
#     categoryField = "movieCategory";
#     forwardAuthGroup = config: config.custom.homelab.groups.admin;
#   })
{
  name,                # Service name (e.g., "radarr", "sonarr")
  port,                # Port number
  description,         # Dashboard description
  rootPath,            # config -> str: Media root folder path
  categoryField,       # Download client category field ("movieCategory" or "tvCategory")
  forwardAuthGroup,    # config -> str: Forward auth group
}:
{ config, pkgs, lib, self, ... }:
let
  upperName = lib.strings.concatImapStrings (i: c: if i == 1 then lib.toUpper c else c) (lib.stringToCharacters name);
  envPrefix = lib.toUpper name;

  serviceCfg = config.custom.homelab.services.${name};
  mediaCfg = config.custom.homelab.media.${name};
  homelabMounts = config.custom.homelab.smb.mounts;
  ntfyCfg = config.custom.homelab.services.ntfy;
  ntfyTags = { radarr = "movie_camera"; sonarr = "tv"; }.${name} or name;

  settings = {
    rootFolders = [{ path = rootPath config; }];
    downloadClient = {
      name = "Transmission";
      host = "127.0.0.1";
      inherit (config.custom.homelab.services.transmission) port;
      urlBase = "/transmission/";
      category = name;
    };
    defaultQualityProfile = mediaCfg.profiles.default.name;
    notification = {
      serverUrl = ntfyCfg.url;
      inherit (serviceCfg.integrations.ntfy) topic;
      tags = ntfyTags;
    };
    defaultDelayProfile = {
      enableUsenet = true;
      enableTorrent = true;
      preferredProtocol = "torrent";
      usenetDelay = 0;
      torrentDelay = 120;
      bypassIfHighestQuality = true;
    };
  };

  settingsFile = pkgs.writeText "${name}-config.json" (builtins.toJSON settings);
in
{
  custom.homelab = {
    services.${name} = {
      displayName = upperName;
      inherit port;
      description = description;
      access.allowedGroups = [ (forwardAuthGroup config) ];
      forwardAuth.enable = true;
      healthcheck.path = "/ping";
      integrations.homepage.enable = true;
      integrations.homepage.tab = "Home";
      integrations.homepage.extraConfig.widget = {
        type = name;
        inherit (serviceCfg) url;
        key = "{{HOMEPAGE_VAR_${lib.toUpper name}_API_KEY}}";
        fields = []; # Omit all fields because we already have the calendar view
      };
      integrations.ntfy.enable = true;
      integrations.ntfy.topic = "media";
      storage.smb = [ "media" ];
    };

    runtimeSecrets."${name}-api-key" = {
      restartUnits = [ "${name}.service" "${name}-configure.service" ];
    };

    runtimeTemplates."${name}.env" = {
      content = ''
        ${envPrefix}__AUTH__APIKEY=${config.custom.homelab.runtimePlaceholder."${name}-api-key"}
      '';
      restartUnits = [ "${name}.service" ];
    };
  };

  services.${name} = {
    enable = true;
    settings.server.port = serviceCfg.port;
    settings.server.bindaddress = "127.0.0.1";
    environmentFiles = [ config.custom.homelab.runtimeTemplates."${name}.env".path ];
  };

  systemd.services.${name} = {
    environment = {
      "${envPrefix}__AUTH__METHOD" = "External";
      "${envPrefix}__LOG__LEVEL" = "info";
    };
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };

  users.users.${name}.extraGroups = [ homelabMounts.media.group ];
  systemd.services."${name}-configure" = {
    description = "${upperName} setup";
    wantedBy = [ "${name}.service" ];
    after = [ "${name}.service" "transmission.service" "recyclarr.service" "ntfy-configure.service" ];
    requires = [ "${name}.service" ];
    wants = [ "transmission.service" "recyclarr.service" "ntfy-configure.service" ];
    partOf = [ "${name}.service" ];
    restartTriggers = [ settingsFile ./arr-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      ARR_NAME = upperName;
      ARR_URL = serviceCfg.url;
      ARR_API_KEY_FILE = config.custom.homelab.runtimeSecrets."${name}-api-key".path;
      ARR_CONFIG_FILE = settingsFile;
      ARR_CATEGORY_FIELD = categoryField;
      NTFY_TOKEN_FILE = serviceCfg.integrations.ntfy.tokenFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "arr-configure" ./arr-configure.nu}'';
  };
}
