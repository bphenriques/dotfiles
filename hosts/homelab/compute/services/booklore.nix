{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.home-server;
  serviceCfg = cfg.services.booklore;
  oidcCfg = cfg.oidc;
  oidcClient = oidcCfg.clients.booklore;
  pathsCfg = config.custom.paths;

  dataDir = "/var/lib/booklore";
  envFile = "/run/booklore/env";
in
{
  custom.home-server.services.booklore = {
    port = 6060;
    dashboard = {
      enable = true;
      category = "Media";
      description = "Book Library";
      icon = "booklore.svg";
    };
  };

  # Booklore uses public OIDC client with PKCE (no client secret required)
  custom.home-server.oidc.clients.booklore = {
    public = true;
    callbackURLs = [ "${serviceCfg.publicUrl}/api/oidc" ];
  };

  sops.secrets = {
    "booklore/db_password" = { };
    "booklore/db_root_password" = { };
  };

  sops.templates."booklore.env".content = ''
    DATABASE_PASSWORD=${config.sops.placeholder."booklore/db_password"}
    MYSQL_PASSWORD=${config.sops.placeholder."booklore/db_password"}
    MYSQL_ROOT_PASSWORD=${config.sops.placeholder."booklore/db_root_password"}
  '';

  systemd.tmpfiles.rules = [
    "d ${dataDir}       0750 root root -"
    "d ${dataDir}/data  0750 root root -"
    "d ${dataDir}/mysql 0750 root root -"
  ];

  virtualisation.oci-containers.containers.booklore-db = {
    image = "lscr.io/linuxserver/mariadb:11.4.5";
    autoStart = true;
    environment = {
      PUID = "1000";
      PGID = "1000";
      TZ = config.time.timeZone;
      MYSQL_DATABASE = "booklore";
      MYSQL_USER = "booklore";
    };
    environmentFiles = [ config.sops.templates."booklore.env".path ];
    volumes = [ "${dataDir}/mysql:/config" ];
    extraOptions = [
      "--network=booklore-net"
      "--memory=512m"
    ];
  };

  virtualisation.oci-containers.containers.booklore = {
    image = "booklore/booklore:v1.16.0";
    autoStart = true;
    dependsOn = [ "booklore-db" ];
    ports = [ "${serviceCfg.internalHost}:${toString serviceCfg.port}:6060" ];

    environment = {
      PUID = "1000";
      PGID = "1000";
      TZ = config.time.timeZone;
      BOOKLORE_PORT = "6060";
      DATABASE_URL = "jdbc:mariadb://booklore-db:3306/booklore";
      DATABASE_USERNAME = "booklore";
      SWAGGER_ENABLED = "false";

      # OIDC (public client with PKCE)
      OIDC_ENABLED = "true";
      OIDC_ISSUER_URI = oidcCfg.provider.url;
      OIDC_SCOPE = "openid profile email offline_access";
      OIDC_USERNAME_CLAIM = "preferred_username";
      OIDC_EMAIL_CLAIM = "email";
      OIDC_DISPLAY_NAME_CLAIM = "name";
    };

    environmentFiles = [
      config.sops.templates."booklore.env".path
      envFile
    ];

    volumes = [
      "${dataDir}/data:/app/data"
      "${pathsCfg.media.books.library}:/data/books"
      "${pathsCfg.media.books.inbox}:/bookdrop"
    ];

    extraOptions = [
      "--network=booklore-net"
      "--read-only"
      "--tmpfs=/tmp:rw,noexec,nosuid,size=128m"
      "--memory=512m"
    ];
  };

  systemd.services.init-booklore-network = {
    description = "Create the network for booklore";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = "${lib.getExe pkgs.podman} network create --ignore booklore-net";
  };

  systemd.services.podman-booklore = {
    after = [
      oidcCfg.systemd.provisionedTarget
      "init-booklore-network.service"
    ];
    wants = [
      oidcCfg.systemd.provisionedTarget
      "init-booklore-network.service"
    ];
    serviceConfig.SupplementaryGroups = [ oidcClient.group ];
    preStart = ''
      mkdir -p "$(dirname "${envFile}")"
      cat > "${envFile}" <<EOF
      OIDC_CLIENT_ID=$(cat ${oidcClient.idFile})
      EOF
      chmod 600 "${envFile}"
    '';
  };

  systemd.services.podman-booklore-db = {
    after = [ "init-booklore-network.service" ];
    wants = [ "init-booklore-network.service" ];
  };
}
