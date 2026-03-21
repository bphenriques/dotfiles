{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.jellyseerr;
  jellyfinCfg = config.custom.homelab.services.jellyfin;
in
{
  imports = [ ./configure.nix ];

  # Auth is delegated to Jellyfin (users sign in via Jellyfin credentials, not direct OIDC/forwardAuth)
  custom.homelab.services.jellyseerr = {
    displayName = "Jellyseerr";
    metadata.category = "Media";
    metadata.description = "TV / Movie Finder";
    metadata.version = config.services.jellyseerr.package.version;
    metadata.homepage = config.services.jellyseerr.package.meta.homepage;
    port = 9099;
    secrets = {
      files.api-key = { rotatable = true; };
      templates."jellyseerr.env".content = ''
        API_KEY=${serviceCfg.secrets.placeholder.api-key}
      '';
      systemd.dependentServices = [ "jellyseerr" "jellyseerr-configure" ];
    };
    healthcheck.path = "/api/v1/status";
    integrations.homepage.enable = true;
  };

  services.jellyseerr = {
    enable = true;
    port = serviceCfg.port;
  };

  systemd.services.jellyseerr.serviceConfig.EnvironmentFile = serviceCfg.secrets.templates."jellyseerr.env".path;
}
