{ config, ... }:
let
  serviceCfg = config.custom.home-server.services.cleanuparr;
  dataDir = "/var/lib/cleanuparr";
in
{
  custom.home-server.services.cleanuparr = {
    port = 11011;
    forwardAuth.enable = true;
    dashboard = {
      enable = true;
      category = "Admin";
      description = "Queue Cleanup";
      icon = "cleanuparr.svg";
    };
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0750 root root -"
  ];

  virtualisation.oci-containers.containers.cleanuparr = {
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
      "--privileged"
      "--memory=256m"
    ];
  };
}
