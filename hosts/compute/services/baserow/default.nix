{ config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.baserow;
  dataDir = "/var/lib/baserow";
  img = pkgs.containerImages.baserow;
in
{
  custom.homelab.services.baserow = {
    displayName = "Baserow";
    metadata.description = "Spreadsheet Database";
    metadata.version = img.version;
    metadata.homepage = img.homepage;
    metadata.category = "Productivity";
    port = 8485;
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    healthcheck.path = "/";
    healthcheck.probeModule = "http_any";
    integrations.homepage.enable = true;

    secrets = {
      files.secret-key = { rotatable = true; bytes = 50; };
      templates.env.content = ''
        SECRET_KEY=${serviceCfg.secrets.placeholder.secret-key}
      '';
      systemd.dependentServices = [ "podman-baserow" ];
    };
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0755 root root -"
  ];

  systemd.services.podman-baserow.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;

  };

  virtualisation.oci-containers.containers.baserow = {
    image = "${img.image}:${img.version}";
    autoStart = true;

    environment = {
      BASEROW_PUBLIC_URL = serviceCfg.publicUrl;
      BASEROW_ENABLE_SECURE_PROXY_SSL_HEADER = "yes";

      # Privacy
      BASEROW_DISABLE_SUPPORT = "yes";

      # Performance
      BASEROW_AMOUNT_OF_GUNICORN_WORKERS = "2";
      BASEROW_AMOUNT_OF_WORKERS = "2";
    };
    environmentFiles = [ serviceCfg.secrets.templates.env.path ];
    ports = [ "127.0.0.1:${toString serviceCfg.port}:80" ];
    volumes = [
      "${dataDir}:/baserow/data"
    ];

    # The all-in-one image runs supervisor as root managing postgres/redis/caddy/backend processes.
    # Requires: no-new-privileges=false + label=disable (Caddy binary has file capabilities),
    # KILL (supervisor signals cross-UID children), NET_BIND_SERVICE (Caddy binds :80).
    user = "0:0";
    extraOptions = [
      "--memory=4g"
      "--pids-limit=-1"
      "--security-opt=no-new-privileges=false"
      "--security-opt=label=disable"
      "--cap-add=CHOWN"
      "--cap-add=DAC_OVERRIDE"
      "--cap-add=FOWNER"
      "--cap-add=SETUID"
      "--cap-add=SETGID"
      "--cap-add=KILL"
      "--cap-add=NET_BIND_SERVICE"
    ];
  };
}
