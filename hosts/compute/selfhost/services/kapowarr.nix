{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.kapowarr;
  pathsCfg = config.custom.paths;

  dataDir = "/var/lib/kapowarr";
  img = pkgs.containerImages.kapowarr;
in
{
  selfhost.services.kapowarr = {
    displayName = "Kapowarr";
    meta.homepage = "https://github.com/Casvt/Kapowarr";
    meta.description = "Comic Book Manager";
    meta.category = "downloads";
    port = 5656;
    access.allowedGroups = with config.selfhost.groups; [ users admin ];
    forwardAuth.enable = true;
    healthcheck.path = "/";
    healthcheck.probeModule = "http_any";
    storage.smb = [ "media" ];
    extraConfig.landingPage.enable = true;
  };

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
      TZ = config.custom.locale.timezone;
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
