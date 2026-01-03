_: {
  services.jellyfin = {
    enable = true;
  };

  services.jellarr = {
    enable = true;
    user = "jellyfin";
    group = "jellyfin";
    environmentFile = config.sops.templates.jellarr-env.path;
    config = {
      base_url = "http://localhost:8096";
      system = {
        enableMetrics = false;
        trickplayOptions = {
          enableHwAcceleration=  true;
          enableHwEncoding = true;
        };
      };
      startup.completeStartupWizard = true;
      library.virtualFolders = [
        {
          name = "Movies";
          collectionType = "movies";
          libraryOptions.pathInfos = [ { path = "/data/movies"; } ];
        }
        {
          name = "TV Shows";
          collectionType = "tvshows";
          libraryOptions.pathInfos = [ { path = "/data/tv"; } ];
        }
      ];
      branding = {
        loginDisclaimer = "Hi! :wave:";
        splashscreenEnabled = false;

        # TODO: check if LG tv works, otherwise apply a fix mentioned here: https://github.com/lscambo13/ElegantFin?tab=readme-ov-file
        customCss = ''
          @import url("https://cdn.jsdelivr.net/gh/lscambo13/ElegantFin@main/Theme/ElegantFin-jellyfin-theme-build-latest-minified.css");
        '';
      };
    };
    users = [
      {
        name = "Bruno";
        passwordFile = "/run/secrets/admin-password";
        policy = {
          isAdministrator = true;
          loginAttemptsBeforeLockout = 3;
        };
      };
    ];
      - name: "regular-user"
        password: "secure-password"
      - name: "viewer-user"
        passwordFile: "/run/secrets/viewer-password"
      - name: "admin-user"
        passwordFile: "/run/secrets/admin-password"
        policy:
          isAdministrator: true
          loginAttemptsBeforeLockout: 3
  };


  # users
  #
  # https://github.com/venkyr77/jellarr
}

#       - "${DATA_DIR}/jellyfin:/config"
  #      - "${MOVIES_DIR}:/data/movies"
  #      - "${TV_SHOWS_DIR}:/data/tvshows"
  #      - "${MUSIC_DIR}:/data/music:ro"
  #      - JELLYFIN_PublishedServerUrl="http://jellyfin.${HOME_SERVER_BASE_URL}"
