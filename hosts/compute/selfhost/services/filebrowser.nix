{ config, ... }:
let
  cfg = config.selfhost;
in
{
  config = {
    selfhost = {
      apps.filebrowser-quantum.enable = true;
      services.filebrowser-quantum = {
        subdomain = "filebrowser"; # the app was renamed, the URL people use was not
        access.allowedGroups = with cfg.groups; [ users admin ];
        traefik.middlewares.filebrowser-buffering.buffering.maxRequestBodyBytes = 4294967296; # 4GB upload cap
        extraConfig.landingPage.enable = true;
      };
    };

    services.filebrowser-quantum.settings.userDefaults.listing = {
      viewMode = "gallery";
      singleClick = true;
      showHidden = false;
    };

  };
}
