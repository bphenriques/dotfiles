# Server setup: Open https://trmnl.<domain>, register and toggle "Permit Auto-Join" toggle.
# Device:
# - Xteink X4: Flash TRMNL firmware via https://trmnl.com/flash (select "X4")
# - Then choose custom server and device should register automatically.
{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.larapaper;
  dataDir = "/var/lib/larapaper";
  img = pkgs.containerImages.larapaper;

  # Alpine www-data UID/GID
  larapaperUser = {
    name = "larapaper";
    uid = 82;
    group = "larapaper";
    gid = 82;
  };
in
{
  custom.homelab.services.larapaper = {
    displayName = "LaraPaper";
    metadata.description = "E-Ink Display Server";
    metadata.version = img.version;
    metadata.homepage = img.homepage;
    metadata.category = "Home";
    port = 4567;
    subdomain = "trmnl";
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    healthcheck.path = "/";
    healthcheck.probeModule = "http_any"; # Redirects to login when unauthenticated
    integrations.homepage.enable = true;

    secrets = {
      files.app-key = { rotatable = false; bytes = 16; };
      templates.env.content = ''
        APP_KEY=${serviceCfg.secrets.placeholder.app-key}
      '';
      systemd.dependentServices = [ "podman-larapaper" ];
    };
  };

  # Device API endpoints bypass ForwardAuth — they use their own token-based auth (access-token header / MAC address).
  services.traefik.dynamicConfigOptions.http = {
    routers.larapaper-device = {
      rule = "Host(`${serviceCfg.publicHost}`) && (Path(`/api/setup`) || Path(`/api/display`) || Path(`/api/log`) || Path(`/api/current_screen`) || PathPrefix(`/storage`))";
      entryPoints = [ "websecure" ];
      service = "larapaper-svc";
      priority = 100;
    };
  };

  users.groups.${larapaperUser.group} = { gid = larapaperUser.gid; };
  users.users.${larapaperUser.name} = {
    uid = larapaperUser.uid;
    group = larapaperUser.group;
    isSystemUser = true;
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir}/database 0750 ${larapaperUser.name} ${larapaperUser.group} -"
    "d ${dataDir}/images   0750 ${larapaperUser.name} ${larapaperUser.group} -"
  ];

  virtualisation.oci-containers.containers.larapaper = {
    image = "${img.image}:${img.version}";
    autoStart = true;

    environment = {
      APP_ENV = "production";
      APP_DEBUG = "false";
      APP_URL = serviceCfg.publicUrl;
      APP_TIMEZONE = config.custom.homelab.locale.timezone;
      PHP_OPCACHE_ENABLE = "1"; # Cache compiled PHP bytecode in memory, avoiding re-parsing on every request
      DB_CONNECTION = "sqlite";
      DB_DATABASE = "database/storage/database.sqlite";
      REGISTRATION_ENABLED = "1"; # web UI is behind ForwardAuth and only for admins.
      FORCE_HTTPS = "1";
      TRUSTED_PROXIES = "*";
      LOG_CHANNEL = "stderr";
      LOG_LEVEL = "warning";
    };
    environmentFiles = [ serviceCfg.secrets.templates.env.path ];
    ports = [ "127.0.0.1:${toString serviceCfg.port}:8080" ];
    volumes = [
      "${dataDir}/database:/var/www/html/database/storage"
      "${dataDir}/images:/var/www/html/storage/app/public/images/generated"
    ];
    # S6 overlay is designed to start as root, set up /run, then drop to www-data.
    # PHP-FPM child processes run as www-data (UID 82), matching the data dir ownership.
    user = "0:0";
    extraOptions = [
      "--memory=512m"
      "--security-opt=no-new-privileges=false"
      "--cap-add=CHOWN"
      "--cap-add=DAC_OVERRIDE"
      "--cap-add=FOWNER"
      "--cap-add=SETUID"
      "--cap-add=SETGID"
      "--cap-add=KILL"
      "--pids-limit=256"
    ];
  };
}
