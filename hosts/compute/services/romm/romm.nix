# Uses host networking to reach MySQL on localhost.
{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.homelab;
  homelabMounts = config.custom.homelab.smb.mounts;
  groupsCfg = config.custom.homelab.groups;

  serviceCfg = cfg.services.romm;
  oidcCfg = cfg.oidc;

  dataDir = "/var/lib/romm";
  img = pkgs.containerImages.romm;

  # Dedicated user for RomM container - member of media group for SMB access
  # GIDs 950 (user), 970 (secrets), 971 (oidc) are fixed for container supplementary group access
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
      multi_file.names = [
        ".stfolder"
        ".idea"
        "media"
      ];
    };
    system.platforms = {
      megadrive = "genesis";
      dreamcast = "dc";
      fbneo = "arcade";
      pico8 = "pico";
      gc = "ngc";
    };

    # Works somewhat but the display is buggy and laggy and only works Chrome<->Chrome. I do not recommend yet.
    emulatorjs.netplay = {
      enabled = true;
      ice_servers =
        let
          port = toString config.services.coturn.listening-port;
          lanIP = self.shared.networks.main.hosts.compute;
          wgIP = "10.100.0.1";
          # Plaintext is acceptable — TURN relay is LAN/WG-only, no internet exposure, credentials are non-sensitive
          turnCreds = { username = "romm"; credential = "romm-netplay"; };
        in [
          { urls = "stun:${lanIP}:${port}"; }
          ({ urls = "turn:${lanIP}:${port}?transport=udp"; } // turnCreds)
          { urls = "stun:${wgIP}:${port}"; }
          ({ urls = "turn:${wgIP}:${port}?transport=udp"; } // turnCreds)
        ];
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
    displayName = "RomM";
    metadata.description = "ROM Manager";
    metadata.version = img.version;
    metadata.homepage = img.homepage;
    metadata.category = "Media";
    port = 8095;

    secrets = {
      gid = 970; # Fixed GIDs required for container supplementary group access (--group-add). Group names are not possible.
      files = {
        auth-secret-key = { rotatable = true; bytes = 64; };
        db-password = { rotatable = false; };
      };
      systemd.dependentServices = [ "podman-romm" "romm-db-setup" ];
      templates.env.content = ''
        ROMM_AUTH_SECRET_KEY=${serviceCfg.secrets.placeholder.auth-secret-key}
      '';
    };
    access.allowedGroups = with cfg.groups; [ guests users admin ];
    oidc = {
      enable = true;
      gid = 971;  # Fixed GID for container access
      callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/openid" ];
      systemd.dependentServices = [ "podman-romm" ];
    };
    healthcheck.path = "/api/heartbeat";
    integrations.homepage.enable = true;
  };

  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "podman-romm" ];

  # Fully own MySQL user creation/password (no ensureUsers - script owns all user state)
  # Assumes NixOS default MySQL root auth: unix socket authentication (no password needed for local root).
  services.mysql.ensureDatabases = [ db.name ];
  systemd.services.romm-db-setup = {
    description = "Setup RomM MySQL user and password";
    wantedBy = [ "multi-user.target" ];
    after = [ "mysql.service" ];
    requires = [ "mysql.service" ];
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
  users.groups.${rommUser.group} = { inherit (rommUser) gid; };
  users.users.${rommUser.name} = {
    inherit (rommUser) uid;
    inherit (rommUser) group;
    isSystemUser = true;
    extraGroups = [ homelabMounts.media.group serviceCfg.secrets.group ];
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir}           0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/resources 0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/redis     0750 ${rommUser.name} ${rommUser.group} -"
    "d ${dataDir}/assets    0750 ${rommUser.name} ${rommUser.group} -"
  ];

  # TODO: Add health check when podman supports healthcheck restart policy
  virtualisation.oci-containers.containers.romm = {
    image = "${img.image}:${img.version}";
    autoStart = true;

    environment = {
      ROMM_PORT = toString serviceCfg.port;  # Host network: bind to configured port
      DISABLE_SETUP_WIZARD = "true";
      HASHEOUS_API_ENABLED = "true";
      KIOSK_MODE = "true";  # Read-only access without login; OIDC users still get full access

      # Auto-scan: picks up new/changed ROMs without manual intervention
      # Filesystem watching uses polling (not inotify) on Podman bind mounts, wasting CPU.
      # The daily scheduled rescan is sufficient.
      ENABLE_RESCAN_ON_FILESYSTEM_CHANGE = "false";
      ENABLE_SCHEDULED_RESCAN = "true";
      SCHEDULED_RESCAN_CRON = "0 3 * * *";

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
      OIDC_SERVER_APPLICATION_URL = oidcCfg.provider.issuerUrl;
      OIDC_CLIENT_ID_FILE = "/run/secrets/oidc_client_id";
      OIDC_CLIENT_SECRET_FILE = "/run/secrets/oidc_client_secret";
      OIDC_CLAIM_ROLES = "groups";
      OIDC_ROLE_ADMIN = groupsCfg.admin;
      OIDC_ROLE_EDITOR = groupsCfg.users;
      OIDC_ROLE_VIEWER = groupsCfg.guests;
    };

    environmentFiles = [ serviceCfg.secrets.templates.env.path ];

    volumes = [
      # Setup
      "${dataDir}/resources:/romm/resources"
      "${dataDir}/redis:/redis-data"
      "${dataDir}/assets:/romm/assets"

      # Secrets
      "${serviceCfg.secrets.files.db-password.path}:/run/secrets/db_password:ro"
      "${serviceCfg.oidc.id.file}:/run/secrets/oidc_client_id:ro"
      "${serviceCfg.oidc.secret.file}:/run/secrets/oidc_client_secret:ro"
      "${config.sops.secrets."romm/mobygames/api-key".path}:/run/secrets/mobygames_api_key:ro"
      "${config.sops.secrets."romm/screenscraper/user".path}:/run/secrets/screenscraper_user:ro"
      "${config.sops.secrets."romm/screenscraper/password".path}:/run/secrets/screenscraper_password:ro"

      # Data
      "${configFile}:/romm/config/config.yml:ro"
      "${config.custom.homelab.paths.media.gaming.emulation.roms}:/romm/library/roms:ro"
      "${config.custom.homelab.paths.media.gaming.emulation.bios}:/romm/library/bios:ro"
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
    restartTriggers = [ configFile ];
  };
}
