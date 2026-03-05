{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.cleanuparr;
  sonarrCfg = config.custom.homelab.services.sonarr;
  radarrCfg = config.custom.homelab.services.radarr;
  transmissionCfg = config.custom.homelab.services.transmission;

  dataDir = "/var/lib/cleanuparr";

  cleanuparrUser = {
    name = "cleanuparr";
    uid = 49031;
    group = "cleanuparr";
    gid = 49031;
  };
in
{
  custom.homelab.services.cleanuparr = {
    port = 11011;
    forwardAuth.enable = true;
    integrations.homepage = {
      enable = true;
      category = "Admin";
      description = "Queue Cleanup";
    };
  };

  users.groups.${cleanuparrUser.group} = { gid = cleanuparrUser.gid; };
  users.users.${cleanuparrUser.name} = {
    uid = cleanuparrUser.uid;
    group = cleanuparrUser.group;
    isSystemUser = true;
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0750 ${cleanuparrUser.name} ${cleanuparrUser.group} -"
  ];

  virtualisation.oci-containers.containers.cleanuparr = {
    image = "ghcr.io/cleanuparr/cleanuparr:2.4.7";
    autoStart = true;
    ports = [ "${serviceCfg.host}:${toString serviceCfg.port}:11011" ];
    environment = {
      PORT = "11011";
      BASE_PATH = "";
      PUID = toString cleanuparrUser.uid;
      PGID = toString cleanuparrUser.gid;
      UMASK = "022";
      TZ = config.time.timeZone;
    };
    volumes = [ "${dataDir}:/config" ];
    extraOptions = [
      "--network=host"
      "--memory=256m"
      "--security-opt=no-new-privileges=false"
      "--cap-add=CHOWN"
      "--cap-add=SETUID"
      "--cap-add=SETGID"
      "--cap-add=DAC_OVERRIDE"
      "--cap-add=FOWNER"
    ];
  };

  systemd.services.podman-cleanuparr = {
    after = [ "sonarr.service" "radarr.service" "transmission.service" ];
    wants = [ "sonarr.service" "radarr.service" "transmission.service" ];
  };
}
