{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.kapowarr;
  pathsCfg = config.custom.homelab.paths;

  dataDir = "/var/lib/kapowarr";
  img = pkgs.containerImages.kapowarr;
in
{
  custom.homelab.services.kapowarr = {
    displayName = "Kapowarr";
    metadata.description = "Comic Book Manager";
    metadata.version = img.version;
    metadata.homepage = "https://github.com/Casvt/Kapowarr"; # FIXME: Add homepage as attribute of the img
    metadata.category = "Media";
    port = 5656;
    access.allowedGroups = with config.custom.homelab.groups; [ users admin ];
    forwardAuth.enable = true;
    healthcheck.path = "/";
    healthcheck.probeModule = "http_any";
    integrations.homepage.enable = true;
  };

  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "podman-kapowarr" ];

  systemd.tmpfiles.rules = [
    "d ${dataDir}           0750 root root -"
    "d ${dataDir}/db        0750 root root -"
    "d ${dataDir}/downloads 0750 root root -"
  ];

  # Kapowarr's entrypoint uses gosu to drop privileges, which resets supplementary groups
  # (--group-add is lost). Running as root (PUID=0) skips gosu entirely and avoids both
  # the chown and group issues. The container is isolated by Podman and behind forwardAuth.
  virtualisation.oci-containers.containers.kapowarr = {
    image = "${img.image}:v${img.version}";
    autoStart = true;

    environment = {
      PUID = "0";
      PGID = "0";
      TZ = config.custom.homelab.locale.timezone;
    };

    volumes = [
      "${dataDir}/db:/app/db"
      "${dataDir}/downloads:/app/temp_downloads"
      "${pathsCfg.media.comics.library}:/comics"
    ];

    ports = [ "127.0.0.1:${toString serviceCfg.port}:5656" ];

    extraOptions = [
      "--memory=512m"
    ];
  };
}
