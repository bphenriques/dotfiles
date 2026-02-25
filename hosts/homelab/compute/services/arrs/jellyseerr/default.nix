{ config, ... }:
let
  serviceCfg = config.custom.home-server.services.jellyseerr;
in
{
  imports = [ ./post-start.nix ];

  custom.home-server.services.jellyseerr = {
    port = 9099;
    dashboard = {
      enable = true;
      category = "Media";
      description = "TV / Movie Finder";
      icon = "jellyseerr.svg";
    };
  };

  sops = {
    secrets."jellyseerr/api-key" = { };
    templates."jellyseerr.env".content = ''
      API_KEY=${config.sops.placeholder."jellyseerr/api-key"}
    '';
  };

  services.jellyseerr = {
    enable = true;
    port = serviceCfg.port;
  };

  systemd.services.jellyseerr.serviceConfig.EnvironmentFile = config.sops.templates."jellyseerr.env".path;
}
