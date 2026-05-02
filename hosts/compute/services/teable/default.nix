{ config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.teable;
  dataDir = "/var/lib/teable";
  img = pkgs.containerImages.teable;

  netName = "teable";
  pgUser = "teable";
  pgDb = "teable";
  pgImage = "postgres:15.4";
  redisImage = "redis:7.2.4";
in
{
  custom.homelab.services.teable = {
    displayName = "Teable";
    metadata.description = "Spreadsheet Database";
    metadata.version = img.version;
    metadata.homepage = img.homepage;
    metadata.category = "Productivity";
    port = 8486;
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    healthcheck.path = "/";
    healthcheck.probeModule = "http_any";
    integrations.homepage.enable = true;

    secrets = {
      files.postgres-password = { rotatable = false; };
      files.redis-password = { rotatable = false; };
      templates.env.content = ''
        POSTGRES_DB=${pgDb}
        POSTGRES_USER=${pgUser}
        POSTGRES_PASSWORD=${serviceCfg.secrets.placeholder.postgres-password}
        REDIS_PASSWORD=${serviceCfg.secrets.placeholder.redis-password}
        PRISMA_DATABASE_URL=postgresql://${pgUser}:${serviceCfg.secrets.placeholder.postgres-password}@teable-db:5432/${pgDb}
        BACKEND_CACHE_REDIS_URI=redis://default:${serviceCfg.secrets.placeholder.redis-password}@teable-cache:6379/0
      '';
      systemd.dependentServices = [
        "podman-teable"
        "podman-teable-db"
        "podman-teable-cache"
      ];
    };
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir}        0755 root root -"
    "d ${dataDir}/data   0755 root root -"
    "d ${dataDir}/db     0755 root root -"
    "d ${dataDir}/cache  0755 root root -"
  ];

  # Bridge network so teable, postgres, and redis can resolve each other by container name.
  systemd.services.podman-network-teable = {
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    after = [ "podman.service" ];
    requires = [ "podman.service" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${pkgs.podman}/bin/podman network exists ${netName} || \
        ${pkgs.podman}/bin/podman network create ${netName}
    '';
  };

  systemd.services.podman-teable = {
    requires = [ "podman-network-teable.service" ];
    after = [ "podman-network-teable.service" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };

  systemd.services.podman-teable-db = {
    requires = [ "podman-network-teable.service" ];
    after = [ "podman-network-teable.service" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
    };
  };

  systemd.services.podman-teable-cache = {
    requires = [ "podman-network-teable.service" ];
    after = [ "podman-network-teable.service" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
    };
  };

  virtualisation.oci-containers.containers.teable-db = {
    image = pgImage;
    autoStart = true;

    environment = {
      TZ = cfg.locale.timezone;
    };
    environmentFiles = [ serviceCfg.secrets.templates.env.path ];
    volumes = [
      "${dataDir}/db:/var/lib/postgresql/data"
    ];
    extraOptions = [
      "--network=${netName}"
      "--network-alias=teable-db"
      "--memory=512m"
    ];
  };

  virtualisation.oci-containers.containers.teable-cache = {
    image = redisImage;
    autoStart = true;

    environmentFiles = [ serviceCfg.secrets.templates.env.path ];
    cmd = [ "sh" "-c" ''redis-server --appendonly yes --requirepass "$REDIS_PASSWORD"'' ];
    volumes = [
      "${dataDir}/cache:/data"
    ];
    extraOptions = [
      "--network=${netName}"
      "--network-alias=teable-cache"
      "--memory=256m"
    ];
  };

  virtualisation.oci-containers.containers.teable = {
    image = "${img.image}:${img.version}";
    autoStart = true;
    dependsOn = [ "teable-db" "teable-cache" ];

    environment = {
      TZ = cfg.locale.timezone;
      PUBLIC_ORIGIN = serviceCfg.publicUrl;
      BACKEND_CACHE_PROVIDER = "redis";
      NEXT_ENV_IMAGES_ALL_REMOTE = "true";
    };
    environmentFiles = [ serviceCfg.secrets.templates.env.path ];
    ports = [ "127.0.0.1:${toString serviceCfg.port}:3000" ];
    volumes = [
      "${dataDir}/data:/app/.assets"
    ];
    extraOptions = [
      "--network=${netName}"
      "--memory=2g"
    ];
  };
}
