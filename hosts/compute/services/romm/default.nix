{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.custom.home-server;
  homelabMounts = config.custom.fileSystems.homelab.mounts;

  serviceCfg = cfg.services.romm;
  oidcCfg = cfg.oidc;
  oidcClient = oidcCfg.clients.romm;

  dataDir = "/var/lib/romm";
  secretsFile = "${dataDir}/secrets.env";

  network = "romm-internal";

  romsDir = config.custom.paths.media.gaming.emulation.roms;
  biosDir = config.custom.paths.media.gaming.emulation.bios;

  db = {
    host = "romm-db";
    name = "romm";
    user = "romm-user";
  };

  yamlFormat = pkgs.formats.yaml { };

  # TODO: Enable netplay once STUN/TURN servers are configured
  # https://github.com/rommapp/romm/releases/tag/4.5.0
  rommConfig = {
    exclude = {
      roms = {
        single_file = {
          extensions = [ "stfolder" ];
          names = [ ".stignore" ];
        };
        multi_file = {
          names = [ ".stfolder" ];
        };
      };
    };
  };

  configFile = yamlFormat.generate "romm-config.yml" rommConfig;

  preStartSecretsScript = pkgs.writeShellApplication {
    name = "romm-pre-start-secrets";
    runtimeInputs = [ pkgs.openssl ];
    text = builtins.readFile ./pre-start-secrets.sh;
  };
in
{
  custom.home-server.services.romm = {
    port = 8095;
    dashboard = {
      enable = true;
      category = "Media";
      description = "ROM Manager";
      icon = "romm.svg";
    };
  };

  custom.home-server.oidc.clients.romm.callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/openid" ];

  systemd.tmpfiles.rules = [
    "d ${dataDir}           0750 root root -"
    "d ${dataDir}/mysql     0755 999 999 -"
    "d ${dataDir}/resources 0750 root root -"
    "d ${dataDir}/redis     0750 root root -"
    "d ${dataDir}/assets    0750 root root -"
  ];

  systemd.services.romm-pre-start-secrets = {
    description = "Generate romm secrets if not present";
    wantedBy = [ "multi-user.target" ];
    before = [
      "podman-romm-db.service"
      "podman-romm.service"
    ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${lib.getExe preStartSecretsScript} ${secretsFile}";
    };
  };

  virtualisation.oci-containers.containers.romm-db = {
    image = "mariadb:11.4.5";
    autoStart = true;
    environment = {
      MYSQL_DATABASE = db.name;
      MYSQL_USER = db.user;
    };
    environmentFiles = [ secretsFile ];
    volumes = [ "${dataDir}/mysql:/var/lib/mysql" ];
    extraOptions = [
      "--network=${network}"
      "--privileged"
      "--memory=512m"
    ];
  };

  virtualisation.oci-containers.containers.romm = {
    image = "rommapp/romm:4.6.0";
    autoStart = true;
    dependsOn = [ "romm-db" ];
    ports = [ "${serviceCfg.internalHost}:${toString serviceCfg.port}:8080" ];

    environment = {
      DISABLE_SETUP_WIZARD = "true";
      HASHEOUS_API_ENABLED = "true";

      DB_HOST = db.host;
      DB_NAME = db.name;
      DB_USER = db.user;

      OIDC_ENABLED = "true";
      DISABLE_USERPASS_LOGIN = "true";
      OIDC_PROVIDER = oidcCfg.provider.displayName; # FIXME
      OIDC_REDIRECT_URI = builtins.head oidcClient.callbackURLs;
      OIDC_SERVER_APPLICATION_URL = oidcCfg.provider.url;
      OIDC_CLIENT_ID_FILE = "/run/secrets/oidc_client_id";
      OIDC_CLIENT_SECRET_FILE = "/run/secrets/oidc_client_secret";
    };

    environmentFiles = [ secretsFile ];

    volumes = [
      "${dataDir}/resources:/romm/resources"
      "${dataDir}/redis:/redis-data"
      "${dataDir}/assets:/romm/assets"
      "${configFile}:/romm/config/config.yml:ro"
      "${romsDir}:/romm/library/roms:ro"
      "${biosDir}:/romm/bios:ro"
      "${oidcClient.idFile}:/run/secrets/oidc_client_id:ro"
      "${oidcClient.secretFile}:/run/secrets/oidc_client_secret:ro"
    ];

    extraOptions = [
      "--network=${network}"
      "--group-add=${toString (config.users.groups.${config.custom.fileSystems.homelab.mounts.media.group}.gid)}"
      "--privileged"
      "--memory=512m"
    ];
  };

  systemd.services.podman-romm = {
    requires = [ homelabMounts.media.automountUnit ];
    after = [
      homelabMounts.media.automountUnit
      oidcCfg.systemd.provisionedTarget
      "romm-pre-start-secrets.service"
    ];
    partOf = [ homelabMounts.media.automountUnit ];
    wants = [
      oidcCfg.systemd.provisionedTarget
      "romm-pre-start-secrets.service"
    ];
    restartTriggers = [ (builtins.toJSON rommConfig) ];
    serviceConfig.SupplementaryGroups = [ oidcClient.group ];
    preStart = "${lib.getExe pkgs.podman} network create --ignore ${network}";
  };

  systemd.services.podman-romm-db = {
    after = [ "romm-pre-start-secrets.service" ];
    wants = [ "romm-pre-start-secrets.service" ];
    preStart = "${lib.getExe pkgs.podman} network create --ignore ${network}";
  };
}
