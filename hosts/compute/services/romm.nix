{ config, pkgs, lib, ... }:
let
  cfg = config.custom.home-server;
  serviceCfg = cfg.routes.romm;
  oidcCfg = cfg.oidc;
  oidcClient = oidcCfg.clients.romm;
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;

  dataDir = "/var/lib/romm";
  envFile = "/run/romm/env";

  romsDir = pathsCfg.media.gaming.emulation.roms;
  biosDir = pathsCfg.media.gaming.emulation.bios;

  configFile = pkgs.writeText "romm-config.yml" ''
    # TODO: Enable netplay once STUN/TURN servers are configured (https://github.com/rommapp/romm/releases/tag/4.5.0)
    # emulatorjs:
    #   netplay:
    #     enabled: true
    #     ice_servers:
    #       - urls: "stun:stun.l.google.com:19302"

    exclude:
      roms:
        single_file:
          extensions: [ 'stfolder' ]
          names: [ ".stignore" ]
        multi_file:
          names: [ ".stfolder" ]
  '';
in
{
  custom.home-server.routes.romm.port = 8095;
  custom.home-server.oidc.clients.romm.callbackURLs = [ "${serviceCfg.publicUrl}/api/oauth/openid" ];

  sops.secrets = {
    "romm/db_password" = { };
    "romm/db_root_password" = { };
    "romm/auth_secret" = { };
    "romm/admin_user" = { };
    "romm/admin_password" = { };
  };

  sops.templates."romm.env".content = ''
    DB_HOST=romm-db
    DB_NAME=romm
    DB_USER=romm-user
    DB_PASSWD=${config.sops.placeholder."romm/db_password"}
    MYSQL_DATABASE=romm
    MYSQL_USER=romm-user
    MYSQL_PASSWORD=${config.sops.placeholder."romm/db_password"}
    MYSQL_ROOT_PASSWORD=${config.sops.placeholder."romm/db_root_password"}
    ROMM_AUTH_SECRET_KEY=${config.sops.placeholder."romm/auth_secret"}
    ROMM_AUTH_USERNAME=${config.sops.placeholder."romm/admin_user"}
    ROMM_AUTH_PASSWORD=${config.sops.placeholder."romm/admin_password"}
  '';

  systemd.tmpfiles.rules = [
    "d ${dataDir}           0750 root root -"
    "d ${dataDir}/mysql     0750 root root -"
    "d ${dataDir}/resources 0750 root root -"
    "d ${dataDir}/redis     0750 root root -"
    "d ${dataDir}/assets    0750 root root -"
  ];

  virtualisation.oci-containers = {
    backend = "podman";

    containers.romm-db = {
      image = "mariadb:11.4.5";
      autoStart = true;
      environmentFiles = [ config.sops.templates."romm.env".path ];
      volumes = [ "${dataDir}/mysql:/var/lib/mysql" ];
      extraOptions = [
        "--network=romm-net"
        "--memory=512m"
      ];
    };

    containers.romm = {
      image = "rommapp/romm:4.6.0";
      autoStart = true;
      dependsOn = [ "romm-db" ];
      ports = [ "${serviceCfg.internalHost}:${toString serviceCfg.port}:8080" ];

      environment = {
        DISABLE_SETUP_WIZARD = "true";
        HASHEOUS_API_ENABLED = "true";

        # OIDC
        OIDC_ENABLED = "true";
        OIDC_PROVIDER = "pocketid";
        OIDC_REDIRECT_URI = builtins.head oidcClient.callbackURLs;
        OIDC_SERVER_APPLICATION_URL = oidcCfg.provider.url;
      };

      environmentFiles = [ config.sops.templates."romm.env".path envFile ];

      volumes = [
        "${dataDir}/resources:/romm/resources"
        "${dataDir}/redis:/redis-data"
        "${dataDir}/assets:/romm/assets"
        "${configFile}:/romm/config/config.yml:ro"
        "${romsDir}:/romm/library/roms:ro"
        "${biosDir}:/romm/bios:ro"
      ];

      extraOptions = [
        "--network=romm-net"
        "--group-add=${toString (config.users.groups.${homelabMounts.media.group}.gid)}"
        "--read-only"
        "--tmpfs=/tmp:rw,noexec,nosuid,size=128m"
        "--memory=512m"
      ];
    };
  };

  systemd.services.init-romm-network = {
    description = "Create the network for romm";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = ''${lib.getExe pkgs.podman} network create --ignore romm-net'';
  };

  systemd.services.podman-romm = {
    after = [ oidcCfg.systemd.provisionedTarget "init-romm-network.service" ];
    wants = [ oidcCfg.systemd.provisionedTarget "init-romm-network.service" ];
    serviceConfig.SupplementaryGroups = [ oidcClient.group ];
    serviceConfig.ExecStartPre = let
      script = pkgs.writeShellScript "romm-env" ''
        mkdir -p "$(dirname "${envFile}")"
        cat > "${envFile}" <<EOF
        OIDC_CLIENT_ID=$(cat ${oidcClient.idFile})
        OIDC_CLIENT_SECRET=$(cat ${oidcClient.secretFile})
        EOF
        chmod 600 "${envFile}"
      '';
    in [ "!${script}" ];
  };

  systemd.services.podman-romm-db = {
    after = [ "init-romm-network.service" ];
    wants = [ "init-romm-network.service" ];
  };
}
