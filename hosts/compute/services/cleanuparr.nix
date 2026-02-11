{ config, ... }:
let
  serviceCfg = config.custom.home-server.routes.cleanuparr;
  dataDir = "/var/lib/cleanuparr";
in
{
  custom.home-server.routes.cleanuparr = {
    port = 11011;
    forwardAuth.enable = true;
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0750 root root -"
  ];

  virtualisation.oci-containers = {
    backend = "podman";
    containers.cleanuparr = {
      image = "ghcr.io/cleanuparr/cleanuparr:2.4.7";
      autoStart = true;
      ports = [ "${serviceCfg.internalHost}:${toString serviceCfg.port}:11011" ];
      environment = {
        PORT = "11011";
        BASE_PATH = "";
        PUID = "1000";
        PGID = "1000";
        UMASK = "022";
        TZ = config.time.timeZone;
      };
      volumes = [ "${dataDir}:/config" ];
      extraOptions = [
        "--read-only"
        "--tmpfs=/tmp:rw,noexec,nosuid,size=64m"
        "--memory=256m"
      ];
    };
  };
}
