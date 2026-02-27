# Creates a standardized *arr service (radarr/sonarr) configuration
#
# Assumptions:
# - Transmission is the only download client used
# - config.custom.homelab.services.transmission.port is defined
# - config.custom.homelab.media.${name}.profiles and defaultProfile exist
#
# Usage:
#   mkArrService {
#     name = "radarr";
#     port = 9098;
#     description = "Movie Tracker";
#     rootPath = config.custom.paths.media.movies;
#     categoryField = "movieCategory";
#     forwardAuthGroup = config.custom.homelab.groups.admin;
#   }
{ config, pkgs, lib, self }:
{
  name,                # Service name (e.g., "radarr", "sonarr")
  port,                # Port number
  description,         # Dashboard description
  rootPath,            # Media root folder path
  categoryField,       # Download client category field ("movieCategory" or "tvCategory")
  forwardAuthGroup,    # Forward auth group (required)
}:
let
  upperName = lib.toUpper (lib.substring 0 1 name) + lib.substring 1 (-1) name;
  envPrefix = lib.toUpper name;

  serviceCfg = config.custom.homelab.services.${name};
  mediaCfg = config.custom.homelab.media.${name};
  homelabMounts = config.custom.fileSystems.homelab.mounts;

  settings = {
    rootFolders = [{ path = rootPath; }];
    downloadClient = {
      inherit name;
      host = "127.0.0.1";
      port = config.custom.homelab.services.transmission.port;
      urlBase = "/transmission/";
      category = name;
    };
    defaultQualityProfile = mediaCfg.profiles.${mediaCfg.defaultProfile}.name;
  };

  settingsFile = pkgs.writeText "${name}-config.json" (builtins.toJSON settings);
in
{
  custom.homelab.services.${name} = {
    inherit port;
    forwardAuth = {
      enable = true;
      group = forwardAuthGroup;
    };
    dashboard = {
      enable = true;
      category = "Media";
      inherit description;
      icon = "${name}.svg";
    };
  };

  sops.secrets."${name}/api-key" = { };
  sops.templates."${name}.env".content = ''
    ${envPrefix}__AUTH__APIKEY=${config.sops.placeholder."${name}/api-key"}
  '';

  services.${name} = {
    enable = true;
    settings.server.port = serviceCfg.port;
    environmentFiles = [ config.sops.templates."${name}.env".path ];
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
  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ name ];

  systemd.services."${name}-configure" = {
    description = "Configure ${upperName} with declarative configuration";
    wantedBy = [ "multi-user.target" ];
    after = [ "${name}.service" "transmission.service" "recyclarr.service" ];
    requires = [ "${name}.service" "recyclarr.service" ];
    wants = [ "transmission.service" "recyclarr.service" ];
    partOf = [ "${name}.service" ];
    restartTriggers = [ settingsFile ./arr-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      ARR_NAME = upperName;
      ARR_URL = serviceCfg.internalUrl;
      ARR_API_KEY_FILE = config.sops.secrets."${name}/api-key".path;
      ARR_CONFIG_FILE = settingsFile;
      ARR_CATEGORY_FIELD = categoryField;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "arr-configure" ./arr-configure.nu}'';
  };
}
