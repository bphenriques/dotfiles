{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.home-server;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
  groupsCfg = config.custom.home-server.groups;

  serviceCfg = cfg.services.romm;
  oidcCfg = cfg.oidc;
  oidcClient = oidcCfg.clients.romm;

  dataDir = "/var/lib/romm";
  secretsFile = "${dataDir}/secrets.env";

  # Dedicated user for RomM container - member of media group for CIFS access
  rommUser = {
    name = "romm";
    uid = 950;
    group = "romm";
    gid = 950;
  };

  db = {
    name = "romm";
    user = "romm";
  };

  # Example: https://github.com/rommapp/romm/blob/master/examples/config.example.yml
  rommConfig = {
    exclude.roms = {
      single_file = {
        extensions = [ "stfolder" ];
        names = [ ".stignore" ];
      };
      multi_file.names = [ ".stfolder" ];
    };
    system.platforms = {
      megadrive = "genesis";
      dreamcast = "dc";
      fbneo = "arcade";
      pico8 = "pico";
    };
  };
  yamlFormat = pkgs.formats.yaml { };
  configFile = yamlFormat.generate "romm-config.yml" rommConfig;
in
{
  imports = [ ./post-start.nix ];

  sops.secrets = {
    "romm/mobygames/api-key" = { };
    "romm/screenscraper/user" = { };
    "romm/screenscraper/password" = { };
  };

  custom.home-server = {
    services.romm = {
      port = 8095;
      dashboard = {
        enable = true;
        category = "Media";
        description = "ROM Manager";
        icon = "romm.svg";
      };
    };
    oidc.clients.romm.callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/openid" ];
  };

  # MySQL database and user for romm
  services.mysql = {
    ensureDatabases = [ db.name ];
    ensureUsers = [{
      name = db.user;
      ensurePermissions = { "${db.name}.*" = "ALL PRIVILEGES"; };
    }];
  };

  # Create dedicated user for RomM container with media group access
  users.groups.${rommUser.group} = { gid = rommUser.gid; };
  users.users.${rommUser.name} = {
    uid = rommUser.uid;
    group = rommUser.group;
    isSystemUser = true;
    extraGroups = [ homelabMounts.media.group "mysql" ];
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir}           0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/resources 0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/redis     0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/assets    0750 ${rommUser.name} ${rommUser.group} -"
  ];

  virtualisation.oci-containers.containers.romm = {
    image = "rommapp/romm:4.6.1";
    autoStart = true;
    ports = [ "${serviceCfg.internalHost}:${toString serviceCfg.port}:8080" ];

    environment = {
      DISABLE_SETUP_WIZARD = "true";
      HASHEOUS_API_ENABLED = "true";
      DISABLE_USERPASS_LOGIN = "false";

      # FIXME: mysql or postgres but then need to figure out authentication. Ideally mysql or postgresqlssh
      # ROMM_DB_DRIVE = "mysql";
      DB_HOST = "localhost";
      DB_NAME = db.name;
      DB_USER = db.user;
      DB_EXTRA_QUERY_PARAMS = builtins.toJSON { unix_socket = "/run/mysqld/mysqld.sock"; };

      # Scrapers
      MOBYGAMES_API_KEY_FILE = "/run/secrets/mobygames_api_key";
      SCREENSCRAPER_USER_FILE = "/run/secrets/screenscraper_user";
      SCREENSCRAPER_PASSWORD_FILE = "/run/secrets/screenscraper_password";

      # OIDC
      OIDC_ENABLED = "true";
      OIDC_PROVIDER = oidcCfg.provider.displayName;
      OIDC_REDIRECT_URI = builtins.head oidcClient.callbackURLs;
      OIDC_SERVER_APPLICATION_URL = oidcCfg.provider.url;
      OIDC_CLIENT_ID_FILE = "/run/secrets/oidc_client_id";
      OIDC_CLIENT_SECRET_FILE = "/run/secrets/oidc_client_secret";
      OIDC_CLAIM_ROLES = "groups";
      OIDC_ROLE_ADMIN = groupsCfg.admin;
      OIDC_ROLE_EDITOR = groupsCfg.admin;
      OIDC_ROLE_VIEWER = groupsCfg.users;
    };

    environmentFiles = [ secretsFile ];

    volumes = [
      # Setup
      "${dataDir}/resources:/romm/resources"
      "${dataDir}/redis:/redis-data"
      "${dataDir}/assets:/romm/assets"

      # MySQL socket for auth (directory mount to preserve permissions)
      "/run/mysqld:/run/mysqld"

      # Secrets
      "${oidcClient.idFile}:/run/secrets/oidc_client_id:ro"
      "${oidcClient.secretFile}:/run/secrets/oidc_client_secret:ro"
      "${config.sops.secrets."romm/mobygames/api-key".path}:/run/secrets/mobygames_api_key:ro"
      "${config.sops.secrets."romm/screenscraper/user".path}:/run/secrets/screenscraper_user:ro"
      "${config.sops.secrets."romm/screenscraper/password".path}:/run/secrets/screenscraper_password:ro"

      # Data
      "${configFile}:/romm/config/config.yml:ro"
      "${config.custom.paths.media.gaming.emulation.roms}:/romm/library/roms:ro"
      "${config.custom.paths.media.gaming.emulation.bios}:/romm/library/bios:ro"
    ];

    user = "${toString rommUser.uid}:${toString rommUser.gid}";
    extraOptions = [
      "--group-add=${toString homelabMounts.media.gid}"
      "--memory=512m"
    ];
  };

  systemd.services.podman-romm = {
    requires = [ homelabMounts.media.automountUnit "mysql.service" ];
    after = [
      homelabMounts.media.automountUnit
      oidcCfg.systemd.provisionedTarget
      "mysql.service"
    ];
    partOf = [ homelabMounts.media.automountUnit ];
    wants = [
      oidcCfg.systemd.provisionedTarget
      "mysql.service"
    ];
    path = [ pkgs.openssl ];
    preStart = ''
      [ -f "${secretsFile}" ] && exit 0
      umask 077
      echo "ROMM_AUTH_SECRET_KEY=$(openssl rand -base64 64 | tr -d '\n')" > "${secretsFile}"
    '';
    restartTriggers = [ (builtins.toJSON rommConfig) ];
    serviceConfig.SupplementaryGroups = [ oidcClient.group ];
  };
}
