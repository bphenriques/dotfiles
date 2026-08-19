{ config, ... }:
{
  selfhost.services.bazarr = {
    meta.category = "media automation";
    storage.mounts = [ "media" ];
    integrations.homepage.group = "Admin";
    extraConfig.landingPage = { enable = true; listed = false; };
  };

  selfhost.apps.bazarr = {
    enable = true;
    configureAfter = [ "sonarr.service" "radarr.service" ];
    sonarr = { };
    radarr = { };

    languageProfiles = [{
      name = "English";
      languages = [ "en" ];
      cutoff = "en";
    }];
    defaultProfile = "English";

    settings = {
      general = {
        enabled_providers = [ "opensubtitlescom" "subf2m" ];

        # Avoid subs burn-in that causes transcoding
        ignore_pgs_subs = true;
        ignore_vobsub_subs = true;
      };

      subf2m.user_agent = "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0"; # Required to avoid rate limiting
    };
    secretSettings = {
      "opensubtitlescom.username" = config.sops.secrets."bazarr/opensubtitles/username".path;
      "opensubtitlescom.password" = config.sops.secrets."bazarr/opensubtitles/password".path;
    };
  };

  sops.secrets."bazarr/opensubtitles/username" = { };
  sops.secrets."bazarr/opensubtitles/password" = { };

  users.users.bazarr.extraGroups = [ config.selfhost.storage.mounts.smb.shares.media.group ];
}
