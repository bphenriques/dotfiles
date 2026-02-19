# OIDC Provisioning Service (users, groups, clients). The service runs on a different host that needs to be secure.
#
# This service runs on boot and provisions:
# - Users from home-server.users (with pocket-id.enable = true)
# - Groups derived from user memberships
# - OIDC clients with credentials in /run/homelab-oidc/

{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.home-server.oidc;
  publicUrl = "https://auth.${self.settings.compute.domain}";

  provisionConfigFile = pkgs.writeText "oidc-provision-config.json" (builtins.toJSON cfg.provisionConfig);
in
{
  # OIDC provider metadata (Pocket-ID running in auth VM)
  custom.home-server.oidc = {
    systemd.provisionedTarget = "homelab-oidc-provision.service";
    provider = {
      displayName = "Pocket-ID";
      internalName = "PocketID";
      url = publicUrl;
      internalUrl = publicUrl;  # Resolves to 10.20.0.10 via /etc/hosts
      discoveryEndpoint = "${publicUrl}/.well-known/openid-configuration";
      apiKeyFile = config.sops.secrets."pocket-id/api-key".path;
    };
  };
  sops.secrets."pocket-id/api-key".mode = "0400";

  assertions = [{
    assertion = cfg.provisionConfig.users != [];
    message = "At least one user must be enabled for Pocket-ID (services.pocket-id.enable = true)";
  }];

  # Base directory for credentials (tmpfs via /run/)
  systemd.tmpfiles.rules = [
    "d ${cfg.credentials.dir} 0750 root root -"
  ];

  # Per-client groups with deterministic GIDs
  users.groups = lib.mkMerge (lib.mapAttrsToList (_: client: {
    ${client.group} = { gid = client.gid; };
  }) cfg.clients);

  systemd.services.homelab-oidc-provision = {
    description = "Pocket-ID Setup";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    requires = [ "microvm@auth.service" ];

    restartTriggers = [ provisionConfigFile ];

    environment = {
      POCKET_ID_URL = publicUrl;         # Use internal URL to distinguish from requests made externally
      POCKET_ID_API_KEY_FILE = cfg.provider.apiKeyFile;
      OIDC_CONFIG_FILE = toString provisionConfigFile;
      OIDC_CREDENTIALS_DIR = cfg.credentials.dir;
    };

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;

      # Security hardening
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      NoNewPrivileges = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
    };

    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "oidc-provision" ./oidc-provision.nu}'';
  };

  # Manual invite script for resending signup emails
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "pocket-id-invite" ''
      export POCKET_ID_URL="${publicUrl}"
      export POCKET_ID_API_KEY_FILE="${cfg.provider.apiKeyFile}"
      exec ${lib.getExe pkgs.nushell} ${self.lib.builders.writeNushellScript "pocket-id-invite" ./pocket-id-invite.nu} "$@"
    '')
  ];
}
