{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.cleanuparr;
  dataDir = "/var/lib/cleanuparr";
  img = pkgs.containerImages.cleanuparr;

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
    metadata.version = img.version;
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
    image = "${img.image}:${img.version}";
    autoStart = true;

    environment = {
      PORT = toString serviceCfg.port;
      BASE_PATH = "";
      PUID = toString cleanuparrUser.uid;
      PGID = toString cleanuparrUser.gid;
      UMASK = "022";
      TZ = config.custom.homelab.locale.timezone;
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
