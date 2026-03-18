# Provisions Pocket-ID users and groups after Pocket-ID starts.
# Writes user mapping to /run/homelab-oidc/oidc-users.json.
# Also provides pocket-id-manage CLI for guest management.
{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.homelab.oidc;
  pocketIdCfg = config.custom.homelab.services.pocket-id;

  configFile = pkgs.writeText "oidc-provision-users-config.json" (builtins.toJSON {
    inherit (cfg.provisionConfig) users groups;
  });

  pocketIdManage = pkgs.writeShellScriptBin "pocket-id-manage" ''
    export POCKET_ID_URL="${pocketIdCfg.url}"
    export POCKET_ID_API_KEY_FILE="${cfg.provider.apiKeyFile}"
    export POCKET_ID_GUESTS_GROUP="${config.custom.homelab.groups.guests}"
    exec ${lib.getExe pkgs.nushell} ${self.lib.builders.writeNushellScript "pocket-id-manage" ./pocket-id-manage.nu} "$@"
  '';
in
{
  # Base directory for credentials (tmpfs via /run/)
  # 0755 allows services to traverse to their subdirectories (per-client dirs are 0750 with group ownership)
  systemd.tmpfiles.rules = [
    "d ${cfg.credentials.dir} 0755 root root -"
  ];

  custom.homelab.oidc.systemd.baseProvisionUnit = "pocket-id-provision-base.service";

  systemd.services.pocket-id-provision-base = {
    description = "Pocket-ID users and groups provisioning";
    wantedBy = [ "pocket-id.service" ];
    after = [ "network-online.target" "pocket-id.service" ];
    wants = [ "network-online.target" ];
    requires = [ "pocket-id.service" ];
    partOf = [ "pocket-id.service" ];
    restartTriggers = [ configFile ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    environment = {
      OIDC_CONFIG_FILE = toString configFile;
      OIDC_CREDENTIALS_DIR = cfg.credentials.dir;
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ReadWritePaths = [ cfg.credentials.dir ];
      ProtectHome = true;
      NoNewPrivileges = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
    };
    path = [ pocketIdManage ];
    script = ''pocket-id-manage provision-users'';
  };

  environment.systemPackages = [ pocketIdManage ];
}
