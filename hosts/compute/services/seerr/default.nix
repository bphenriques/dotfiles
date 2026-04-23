{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.seerr;
in
{
  imports = [ ./configure.nix ];

  # Auth is delegated to Jellyfin (users sign in via Jellyfin credentials, not direct OIDC/forwardAuth)
  custom.homelab.services.seerr = {
    displayName = "Seerr";
    metadata.category = "Media";
    metadata.description = "TV / Movie Finder";
    metadata.version = config.services.seerr.package.version;
    metadata.homepage = config.services.seerr.package.meta.homepage;
    port = 9099;
    secrets = {
      files.api-key = { rotatable = true; };
      templates."seerr.env".content = ''
        API_KEY=${serviceCfg.secrets.placeholder.api-key}
      '';
      systemd.dependentServices = [ "seerr" "seerr-configure" ];
    };
    healthcheck.path = "/api/v1/status";
    integrations.homepage.enable = true;
  };

  services.seerr = {
    enable = true;
    inherit (serviceCfg) port;
  };

  systemd.services.seerr.serviceConfig.EnvironmentFile = serviceCfg.secrets.templates."seerr.env".path;
}
