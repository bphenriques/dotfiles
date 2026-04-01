{ config, ... }:
let
  version = "v3.4.6";
in {
  virtualisation.oci-containers.containers.flaresolverr = {
    image = "ghcr.io/flaresolverr/flaresolverr:${version}";
    autoStart = true;
    environment = {
      HOST = "127.0.0.1";
      PORT = "8191";
      LOG_LEVEL = "info";
      TZ = config.custom.homelab.locale.timezone;
    };
    extraOptions = [
      "--network=host"
      "--memory=512m"
    ];
  };

  systemd.services.podman-flaresolverr = {
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
  };
}
