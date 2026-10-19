{ config, ... }:
{
  selfhost.services.bazarr = {
    meta.category = "media automation";
    storage.mounts = [ "media" ];
    storage.users = [ "bazarr" ];
    integrations.homepage.group = "Admin";
    extraConfig.landingPage = { enable = true; listed = false; };
  };

  selfhost.apps.bazarr = {
    enable = true;
    sonarr = { };
    radarr = { };

    languageProfiles = [{
      name = "English";
      languages = [ "en" ];
      cutoff = "en";
    }];
    defaultProfile = "English";
  };

  services.bazarr.settings = {
    general = {
      enabled_providers = [ "opensubtitlescom" "subf2m" ];

      # Avoid subs burn-in that causes transcoding
      ignore_pgs_subs = true;
      ignore_vobsub_subs = true;
    };

    subf2m.user_agent = "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0"; # Required to avoid rate limiting

    opensubtitlescom = {
      username._secret = config.sops.secrets."bazarr/opensubtitles/username".path;
      password._secret = config.sops.secrets."bazarr/opensubtitles/password".path;
    };
  };

  sops.secrets."bazarr/opensubtitles/username" = { };
  sops.secrets."bazarr/opensubtitles/password" = { };

}
