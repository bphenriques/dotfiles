# Pocket-ID OIDC Provisioning
#
# Provisions users, groups, and OIDC clients after Pocket-ID starts.
# Credentials are written to /run/homelab-oidc/<client>/{id,secret}.

{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.homelab.oidc;
  provisionConfigFile = pkgs.writeText "oidc-provision-config.json" (builtins.toJSON cfg.provisionConfig);
  pocketIdCfg = config.custom.homelab.services.pocket-id;
in
{
  assertions = [{
    assertion = cfg.provisionConfig.users != [];
    message = "At least one user must be enabled for Pocket-ID (services.pocket-id.enable = true)";
  }];

  # Base directory for credentials (tmpfs via /run/)
  # 0755 allows services to traverse to their subdirectories (per-client dirs are 0750 with group ownership)
  systemd.tmpfiles.rules = [
    "d ${cfg.credentials.dir} 0755 root root -"
  ];

  # Per-client groups with deterministic GIDs
  users.groups = lib.mkMerge (lib.mapAttrsToList (_: client: {
    ${client.group} = { gid = client.gid; };
  }) cfg.clients);


  custom.homelab.oidc.systemd.provisionedTarget = "pocket-id-provision.service";
  systemd.services.pocket-id-provision = let
    script = self.lib.builders.writeNushellScript "oidc-provision" ./oidc-provision.nu;
  in {
    description = "Pocket-ID Provisioning";
    wantedBy = [ "pocket-id.service" ];
    after = [ "network-online.target" "pocket-id.service" ];
    wants = [ "network-online.target" ];
    requires = [ "pocket-id.service" ];
    partOf = [ "pocket-id.service" ];
    restartTriggers = [ provisionConfigFile ./oidc-provision.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;

    environment = {
      POCKET_ID_URL = pocketIdCfg.internalUrl;
      POCKET_ID_API_KEY_FILE = cfg.provider.apiKeyFile;
      OIDC_CONFIG_FILE = toString provisionConfigFile;
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

    path = [ pkgs.nushell ];
    script = ''nu ${script}'';
  };

  # Manual invite script
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "pocket-id-invite" ''
      export POCKET_ID_URL="${pocketIdCfg.internalUrl}"
      export POCKET_ID_API_KEY_FILE="${cfg.provider.apiKeyFile}"
      exec ${lib.getExe pkgs.nushell} ${self.lib.builders.writeNushellScript "pocket-id-invite" ./pocket-id-invite.nu} "$@"
    '')
  ];
}
