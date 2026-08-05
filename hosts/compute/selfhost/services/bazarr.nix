{ config, ... }:
{
  selfhost.apps.bazarr = {
    enable = true;
    configureAfter = [ "sonarr.service" "radarr.service" ];
    sonarr = {
      inherit (config.selfhost.services.sonarr) port;
      inherit (config.selfhost.apps.sonarr) apiKeyFile;
    };
    radarr = {
      inherit (config.selfhost.services.radarr) port;
      inherit (config.selfhost.apps.radarr) apiKeyFile;
    };

    languageProfiles = [{
      name = "English";
      languages = [ "en" ];
      cutoff = "en";
    }];
    defaultProfile = "English";

    settings = {
      general.enabled_providers = [ "opensubtitlescom" "subf2m" ];
      # subf2m refuses to run without one, throttling itself for 12h on every attempt.
      subf2m.user_agent = "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0";
    };
    secretSettings = {
      "opensubtitlescom.username" = config.sops.secrets."bazarr/opensubtitles/username".path;
      "opensubtitlescom.password" = config.sops.secrets."bazarr/opensubtitles/password".path;
    };
  };

  selfhost.services.bazarr = {
    meta.category = "media automation";
    storage.smb = [ "media" ];
    integrations.homepage.group = "Admin";
    extraConfig.landingPage = { enable = true; listed = false; };
  };

  sops.secrets."bazarr/opensubtitles/username" = { };
  sops.secrets."bazarr/opensubtitles/password" = { };

  users.users.bazarr.extraGroups = [ config.selfhost.storage.smb.mounts.media.group ];
}
