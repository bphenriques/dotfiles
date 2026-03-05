{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.homelab;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
  groupsCfg = config.custom.homelab.groups;

  serviceCfg = cfg.services.romm;
  oidcCfg = cfg.oidc;

  dataDir = "/var/lib/romm";
  envFile = "/run/romm/romm.env";

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
  yamlFormat = pkgs.formats.yaml { };
  configFile = yamlFormat.generate "romm-config.yml" {
    exclude.roms = {
      single_file = {
        extensions = [ "stfolder" ];
        names = [ ".stignore" ];
      };
      multi_file.names = [ ".stfolder" ]; # syncthing
    };
    system.platforms = {
      megadrive = "genesis";
      dreamcast = "dc";
      fbneo = "arcade";
      pico8 = "pico";
    };
  };
in
{
  sops.secrets = {
    "romm/mobygames/api-key" = { owner = rommUser.name; };
    "romm/screenscraper/user" = { owner = rommUser.name; };
    "romm/screenscraper/password" = { owner = rommUser.name; };
  };

  custom.homelab.services.romm = {
    port = 8095;
    integrations.homepage = {
      enable = true;
      category = "Media";
      description = "ROM Manager";
    };
    secrets = {
      gid = 970;  # Fixed GID for container access
      files = {
        auth-secret-key = { rotatable = true; length = 64; };
        db-password = { rotatable = false; };
      };
      systemd.dependentServices = [ "podman-romm" ];
    };
    oidc = {
      enable = true;
      callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/openid" ];
      systemd.dependentServices = [ "podman-romm" ];
    };
  };

  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "podman-romm" ];

  # Fully own MySQL user creation/password (no ensureUsers - script owns all user state)
  services.mysql.ensureDatabases = [ db.name ];
  systemd.services.romm-db-setup = {
    description = "Setup RomM MySQL user and password";
    wantedBy = [ "multi-user.target" ];
    after = [ "mysql.service" "homelab-secrets-romm.service" ];
    requires = [ "mysql.service" "homelab-secrets-romm.service" ];
    before = [ "podman-romm.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      set -euo pipefail
      PASSWORD=$(cat ${serviceCfg.secrets.files.db-password.path})
      # Idempotent: create user if missing, set password, grant privileges
      # Password is hex-only (openssl rand -hex), so SQL interpolation is safe
      ${config.services.mysql.package}/bin/mysql -u root <<SQL
        CREATE USER IF NOT EXISTS '${db.user}'@'localhost' IDENTIFIED BY '$PASSWORD';
        ALTER USER '${db.user}'@'localhost' IDENTIFIED BY '$PASSWORD';
        GRANT ALL PRIVILEGES ON \`${db.name}\`.* TO '${db.user}'@'localhost';
      SQL
    '';
  };

  # Create dedicated user for RomM container with media group access
  users.groups.${rommUser.group} = { gid = rommUser.gid; };
  users.users.${rommUser.name} = {
    uid = rommUser.uid;
    group = rommUser.group;
    isSystemUser = true;
    extraGroups = [ homelabMounts.media.group serviceCfg.secrets.group ];
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir}           0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/resources 0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/redis     0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/assets    0750 ${rommUser.name} ${rommUser.group} -"
  ];

  virtualisation.oci-containers.containers.romm = {
    image = "rommapp/romm:4.7.0";
    autoStart = true;

    environment = {
      ROMM_PORT = toString serviceCfg.port;  # Host network: bind to configured port
      DISABLE_SETUP_WIZARD = "true";
      HASHEOUS_API_ENABLED = "true";
      DISABLE_USERPASS_LOGIN = "true";

      DB_HOST = "127.0.0.1";  # Host MySQL via host network
      DB_NAME = db.name;
      DB_USER = db.user;
      DB_PASSWD_FILE = "/run/secrets/db_password";
      DB_PORT = "3306";

      # Scrapers
      MOBYGAMES_API_KEY_FILE = "/run/secrets/mobygames_api_key";
      SCREENSCRAPER_USER_FILE = "/run/secrets/screenscraper_user";
      SCREENSCRAPER_PASSWORD_FILE = "/run/secrets/screenscraper_password";

      # OIDC
      OIDC_ENABLED = "true";
      OIDC_PROVIDER = oidcCfg.provider.displayName;
      OIDC_REDIRECT_URI = builtins.head serviceCfg.oidc.callbackURLs;
      OIDC_SERVER_APPLICATION_URL = oidcCfg.provider.url;
      OIDC_CLIENT_ID_FILE = "/run/secrets/oidc_client_id";
      OIDC_CLIENT_SECRET_FILE = "/run/secrets/oidc_client_secret";
      OIDC_CLAIM_ROLES = "groups";
      OIDC_ROLE_ADMIN = groupsCfg.admin;
      OIDC_ROLE_EDITOR = groupsCfg.admin;
      OIDC_ROLE_VIEWER = groupsCfg.users;
    };

    environmentFiles = [ envFile ];

    volumes = [
      # Setup
      "${dataDir}/resources:/romm/resources"
      "${dataDir}/redis:/redis-data"
      "${dataDir}/assets:/romm/assets"

      # Secrets
      "${serviceCfg.secrets.files.db-password.path}:/run/secrets/db_password:ro"
      "${serviceCfg.oidc.idFile}:/run/secrets/oidc_client_id:ro"
      "${serviceCfg.oidc.secretFile}:/run/secrets/oidc_client_secret:ro"
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
      "--network=host"  # Use host network for MySQL access
      "--group-add=${toString homelabMounts.media.gid}"
      "--group-add=${toString serviceCfg.oidc.gid}"
      "--group-add=${toString serviceCfg.secrets.gid}"
      "--memory=512m"
    ];
  };

  systemd.services.podman-romm = {
    requires = [ "romm-db-setup.service" ];
    after = [ "romm-db-setup.service" ];
    wants = [ "romm-db-setup.service" ];
    serviceConfig.RuntimeDirectory = "romm";
    preStart = ''
      echo "ROMM_AUTH_SECRET_KEY=$(cat ${serviceCfg.secrets.files.auth-secret-key.path})" > "${envFile}"
      chmod 600 "${envFile}"
    '';
    restartTriggers = [ configFile ];
  };
}
