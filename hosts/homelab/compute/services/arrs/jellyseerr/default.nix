{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyseerr;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.jellyseerr = {
    port = 9099;
    secrets = {
      files.api-key = { rotatable = true; };
      templates."jellyseerr.env".content = ''
        API_KEY=${serviceCfg.secrets.placeholder.api-key}
      '';
      systemd.dependentServices = [ "jellyseerr" "jellyseerr-configure" ];
    };

    # TODO: https://gethomepage.dev/widgets/services/jellyseerr/
    integrations.homepage = {
      enable = true;
      category = "Media";
      description = "TV / Movie Finder";
    };
  };

  services.jellyseerr = {
    enable = true;
    port = serviceCfg.port;
  };

  systemd.services.jellyseerr.serviceConfig.EnvironmentFile = serviceCfg.secrets.templates."jellyseerr.env".path;
}
