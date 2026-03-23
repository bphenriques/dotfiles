{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.cleanuparr;
  dataDir = "/var/lib/cleanuparr";
  version = "2.4.7";

  cleanuparrUser = {
    name = "cleanuparr";
    uid = 49031;
    group = "cleanuparr";
    gid = 49031;
  };
in
{
  custom.homelab.services.cleanuparr = {
    displayName = "Cleanuparr";
    metadata.description = "Queue Cleanup";
    metadata.version = version;
    metadata.homepage = "https://github.com/cleanuparr/cleanuparr";
    metadata.category = "Media";
    port = 11011;
    healthcheck.path = "/api/health";
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.homepage.tab = "Admin";
    integrations.homepage.icon = "cleanuparr.png";
  };

  users.groups.${cleanuparrUser.group} = { gid = cleanuparrUser.gid; };
  users.users.${cleanuparrUser.name} = {
    uid = cleanuparrUser.uid;
    group = cleanuparrUser.group;
    isSystemUser = true;
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0755 ${cleanuparrUser.name} ${cleanuparrUser.group} -"
  ];

  virtualisation.oci-containers.containers.cleanuparr = {
    image = "ghcr.io/cleanuparr/cleanuparr:${version}";
    autoStart = true;

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
      # Security exception: LinuxServer-style container requires elevated privileges at startup (PUID/PGID to setuid/chown).
      "--security-opt=no-new-privileges=false"
      "--cap-add=CHOWN"
      "--cap-add=SETUID"
      "--cap-add=SETGID"
    ];
  };

  systemd.services.podman-cleanuparr = {
    after = [ "sonarr.service" "radarr.service" "transmission.service" ];
    wants = [ "sonarr.service" "radarr.service" "transmission.service" ];
  };
}
