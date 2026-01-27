{ config, pkgs, lib, ... }:
let
  cfg = config.custom.home-server;
  pathsCfg = config.custom.paths;

  emulationDir = pathsCfg.media.gaming.emulation;
  dataDir = "/var/lib/romm";

  folders = [
    "bios" "gb" "gba" "gbc" "dos" "dreamcast" "fbneo"
    "megadrive" "n64" "nds" "nes" "psp" "psx" "snes"
  ];
  mkMount = folders: "${emulationDir}/${folders}:/romm/library/${folders}:ro";

in {
  custom.home-server.routes.romm = {
    port = 8095;
    oidc.enable = true;
    callbackPath = "/api/oauth/openid";
  };

  # 2. SOPS Secrets & Environment
  # We consolidate all env vars here to inject them into the container
  sops.templates."romm.env".content = ''
    # --- Database Config ---
    DB_HOST=romm-db
    DB_NAME=romm
    DB_USER=romm-user
    DB_PASSWD=${config.sops.placeholder.romm_db_password}

    MYSQL_DATABASE=romm
    MYSQL_USER=romm-user
    MYSQL_PASSWORD=${config.sops.placeholder.romm_db_password}
    MYSQL_ROOT_PASSWORD=${config.sops.placeholder.romm_db_root_password}

    # --- App Config ---
    ROMM_AUTH_SECRET_KEY=${config.sops.placeholder.romm_auth_secret}
    ROMM_AUTH_USERNAME=${config.sops.placeholder.romm_admin_user}
    ROMM_AUTH_PASSWORD=${config.sops.placeholder.romm_admin_password}

    # --- OIDC Config ---
    OIDC_CLIENT_ID=romm
    OIDC_CLIENT_SECRET=${config.sops.placeholder.oidc_secret_romm}
    OIDC_REDIRECT_URI=${cfg.services.romm.publicUrl}/api/oauth/openid
    OIDC_SERVER_APPLICATION_URL=${cfg.services.pocket-id.publicUrl}
  '';

  # Define the placeholder secrets in your sops.yaml
  sops.secrets = {
    romm_db_password = {};
    romm_db_root_password = {};
    romm_auth_secret = {}; # Generate this with `openssl rand -hex 32`
    romm_admin_user = {};
    romm_admin_password = {};
    oidc_secret_romm = {}; # Generate this for the OIDC handshake
  };

  virtualisation.oci-containers = {
    backend = "podman";

    containers = {
      romm-db = {
        image = "mariadb:latest";
        autoStart = true;
        environmentFiles = [ config.sops.templates."romm.env".path ];
        volumes = [ "${dataDir}/mysql:/var/lib/mysql" ];
        extraOptions = [ "--network=romm-net" ];
      };

      romm = {
        image = "rommapp/romm:4.4.1";
        autoStart = true;
        dependsOn = [ "romm-db" ];

        ports = [ "${custom.home-server.routes.romm.port}:8080" ];

        environment = {
          HASHEOUS_API_ENABLED = "true";
          DISABLE_SETUP_WIZARD = "true";
          OIDC_ENABLED = "true";
          OIDC_PROVIDER = "pocketid";
        };

        environmentFiles = [ config.sops.templates."romm.env".path ];

        volumes = [
          # App Data
          "${dataDir}/resources:/romm/resources"
          "${dataDir}/redis:/redis-data"
          "${dataDir}/assets:/romm/assets"
          "${dataDir}/config:/romm/config"
        ] ++ (map mkMount folders); # Append the ROM mounts generated above

        # Connect to the custom network and set the user ID (PUID:PGID)
        # Assuming 1000:100 is your main user/group. Adjust if needed.
        extraOptions = [ "--network=romm-net" "--user=1000:100" ];
      };
    };
  };

  systemd.services.init-romm-network = {
    description = "Create the network for romm";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = ''${lib.getExe pkgs.podman} network create --ignore romm-net'';
  };
}