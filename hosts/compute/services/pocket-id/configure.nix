# Pocket-ID provisioning: users, groups, and per-client OIDC credentials.
# - Users/groups are provisioned first (pocket-id-provision-base).
# - Each OIDC client gets its own systemd unit for independent rotation.
# - Credentials are written to /run/homelab-oidc/<client>/{id,secret}.
# - Provides pocket-id-manage CLI for guest management.
{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.homelab.oidc;
  pocketIdCfg = config.custom.homelab.services.pocket-id;
  baseServiceName = "pocket-id-provision-base";

  usersConfigFile = pkgs.writeText "oidc-provision-users-config.json" (builtins.toJSON {
    inherit (cfg.provisionConfig) users groups;
  });

  mkClientConfigFile = name: client:
    pkgs.writeText "oidc-client-config-${name}.json" (builtins.toJSON {
      inherit (client) name callbackURLs pkce allowedGroups;
    });

  pocketIdManage = pkgs.writeShellApplication {
    name = "pocket-id-manage";
    runtimeInputs = [ self.packages.pocket-id-manage ];
    text = ''
      export POCKET_ID_URL="${pocketIdCfg.url}"
      export POCKET_ID_API_KEY_FILE="${cfg.provider.apiKeyFile}"
      export POCKET_ID_GUESTS_GROUP="${config.custom.homelab.groups.guests}"
      exec pocket-id-manage-bin "$@"
    '';
  };

  hardenedServiceConfig = {
    Type = "oneshot";
    RemainAfterExit = true;
    Restart = "on-failure";
    RestartSec = 10;
    UMask = "0077";
    PrivateTmp = true;
    ProtectSystem = "strict";
    ReadWritePaths = [ cfg.credentials.dir ];
    ProtectHome = true;
    NoNewPrivileges = true;
    ProtectKernelTunables = true;
    ProtectControlGroups = true;
    RestrictSUIDSGID = true;
    RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
  };
in
{
  # Base directory for credentials (tmpfs via /run/)
  # 0755 allows services to traverse to their subdirectories (per-client dirs are 0750 with group ownership)
  systemd.tmpfiles.rules = [
    "d ${cfg.credentials.dir} 0755 root root -"
  ];

  custom.homelab.oidc.systemd = {
    baseProvisionUnit = "${baseServiceName}.service";
    clientProvisionUnitPrefix = "pocket-id-provision-client-";
  };

  systemd.services = {
    ${baseServiceName} = {
      description = "Pocket-ID users and groups provisioning";
      wantedBy = [ "pocket-id.service" ];
      after = [ "network-online.target" "pocket-id.service" ];
      wants = [ "network-online.target" ];
      requires = [ "pocket-id.service" ];
      partOf = [ "pocket-id.service" ];
      restartTriggers = [ usersConfigFile ];
      startLimitIntervalSec = 300;
      startLimitBurst = 3;
      environment = {
        OIDC_CONFIG_FILE = toString usersConfigFile;
        OIDC_CREDENTIALS_DIR = cfg.credentials.dir;
      };
      serviceConfig = hardenedServiceConfig;
      path = [ pocketIdManage ];
      script = ''pocket-id-manage provision-users'';
    };
  } // lib.mapAttrs' (name: client: let
    clientConfigFile = mkClientConfigFile name client;
  in lib.nameValuePair "pocket-id-provision-client-${name}" {
    description = "Pocket-ID OIDC client provisioning for ${name}";
    wantedBy = [ "pocket-id.service" ];
    after = [ "network-online.target" "pocket-id.service" "${baseServiceName}.service" ];
    wants = [ "network-online.target" ];
    requires = [ "pocket-id.service" "${baseServiceName}.service" ];
    partOf = [ "pocket-id.service" ];
    restartTriggers = [ clientConfigFile ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    environment = {
      OIDC_CLIENT_CONFIG_FILE = toString clientConfigFile;
      OIDC_CREDENTIALS_DIR = cfg.credentials.dir;
    };
    serviceConfig = hardenedServiceConfig;
    path = [ pocketIdManage ];
    script = ''pocket-id-manage provision-client'';
  }) cfg.clients;

  environment.systemPackages = [ pocketIdManage ];
}
