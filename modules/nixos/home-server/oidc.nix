# OIDC Credential Management
#
# This module provisions OIDC clients with Pocket-ID and manages credentials.
#
# Flow:
#   1. On boot, homelab-oidc-provision.service calls Pocket-ID API
#   2. For each client: ensure it exists, regenerate secret, write to /run/homelab-oidc/{client}/
#   3. Client services depend on homelab-oidc-ready.target
#   4. Client services read credentials from their respective files (idFile, secretFile)
#
# Credentials are stored in tmpfs (/run/) and regenerated on every boot.
# For a 24/7 server, this means secrets rotate only on reboot (rare).
# Manual rotation: systemctl restart homelab-oidc-provision && restart dependent services.
#
# Per-client groups:
#   Each client gets its own group (homelab-oidc-{name}) with a deterministic GID based on
#   a hash of the client name (base 42000 + first 4 hex chars of SHA256). This ensures:
#   - Consistent GIDs across hosts for multi-host setups
#   - Per-service isolation (only services in the group can read credentials)
#
# Consumer pattern:
#   systemd.services.<name> = {
#     wants = [ oidcCfg.systemd.provisionedTarget ];
#     after = [ oidcCfg.systemd.provisionedTarget ];
#     serviceConfig.SupplementaryGroups = [ oidcClient.group ];
#   };
#
{ lib, config, pkgs, self, ... }:
let
  homeServerCfg = config.custom.home-server;
  cfg = homeServerCfg.oidc;
  credentialsBaseDir = "/run/homelab-oidc";

  # Deterministic GID: base + hash offset (0-65535 range)
  baseGid = 42000;
  hashOffset = name: lib.fromHexString (builtins.substring 0 4 (builtins.hashString "sha256" name));
  clientGid = name: baseGid + (hashOffset name);
  clientGroup = name: "homelab-oidc-${name}";

  clientOpt = lib.types.submodule ({ name, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Name of the OIDC client in Pocket-ID";
      };

      callbackURLs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "${homeServerCfg.services.${name}.publicUrl}/oauth2/oidc/callback" ];
        description = "Callback URLs for the OIDC client";
      };

      pkce = lib.mkOption { type = lib.types.bool; default = false; };

      group = lib.mkOption {
        type = lib.types.str;
        default = clientGroup name;
        readOnly = true;
        description = "Group name for this client's credentials (add to service's SupplementaryGroups)";
      };

      credentialsDir = lib.mkOption {
        type = lib.types.str;
        default = "${credentialsBaseDir}/${name}";
        readOnly = true;
        description = "Directory containing this client's id and secret files";
      };

      idFile = lib.mkOption {
        type = lib.types.str;
        default = "${credentialsBaseDir}/${name}/id";
        readOnly = true;
        description = "Path to the file containing the client ID";
      };

      secretFile = lib.mkOption {
        type = lib.types.str;
        default = "${credentialsBaseDir}/${name}/secret";
        readOnly = true;
        description = "Path to the file containing the client secret";
      };
    };
  });

  # Generate config file for the provisioning script
  provisionConfigFile = pkgs.writeText "oidc-provision-config.json" (builtins.toJSON {
    clients = lib.mapAttrsToList (_: client: { inherit (client) name callbackURLs pkce; }) cfg.clients;
  });
in
{
  options.custom.home-server.oidc = {
    provider = {
      displayName = lib.mkOption {
        type = lib.types.str;
        description = "Display name of the OIDC provider (shown in UI, e.g., 'Pocket-ID')";
      };

      internalName = lib.mkOption {
        type = lib.types.str;
        description = "Internal name for URLs and identifiers (no special characters, e.g., 'PocketID')";
      };

      url = lib.mkOption {
        type = lib.types.str;
        description = "Public URL of the OIDC provider";
      };

      discoveryEndpoint = lib.mkOption {
        type = lib.types.str;
        description = "OIDC discovery endpoint URL";
      };

      apiKeyFile = lib.mkOption {
        type = lib.types.str;
        description = "Path to file containing Pocket-ID API key";
      };
    };

    credentials = {
      dir = lib.mkOption {
        type = lib.types.str;
        default = credentialsBaseDir;
        readOnly = true;
        description = "Base directory for OIDC credentials (tmpfs)";
      };

      usersFile = lib.mkOption {
        type = lib.types.str;
        default = "${credentialsBaseDir}/oidc-users.json";
        readOnly = true;
        description = "JSON file mapping usernames to their OIDC provider user IDs";
      };
    };

    clients = lib.mkOption {
      type = lib.types.attrsOf clientOpt;
      default = { };
      description = "OIDC clients to register with the provider";
    };

    systemd = {
      provisionedTarget = lib.mkOption {
        type = lib.types.str;
        default = "homelab-oidc-ready.target";
        readOnly = true;
        description = "Systemd target indicating OIDC credentials are ready. Use in both Wants and After.";
      };
    };
  };

  config = lib.mkIf (cfg.clients != { }) {
    # Per-client groups with deterministic GIDs
    users.groups = lib.mkMerge (lib.mapAttrsToList (name: _: {
      ${clientGroup name} = { gid = clientGid name; };
    }) cfg.clients);

    # Base directory for credentials (tmpfs via /run/)
    systemd.tmpfiles.rules = [
      "d ${credentialsBaseDir} 0750 root root -"
    ];

    # Provisioning service that calls Pocket-ID API
    systemd.services.homelab-oidc-provision = {
      description = "Provision OIDC clients with Pocket-ID";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      # Re-run when config changes
      restartTriggers = [ provisionConfigFile ];

      environment = {
        POCKET_ID_URL = cfg.provider.url;
        POCKET_ID_API_KEY_FILE = cfg.provider.apiKeyFile;
        OIDC_CONFIG_FILE = toString provisionConfigFile;
        OIDC_CREDENTIALS_DIR = credentialsBaseDir;
      };

      path = [ pkgs.nushell ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = ''nu ${self.lib.builders.writeNushellScript "oidc-provision" ./oidc-provision.nu}'';

        # Security hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
      };
    };

    # Target that indicates OIDC credentials are ready
    systemd.targets."homelab-oidc-ready" = {
      description = "OIDC credentials are ready";
      requires = [ "homelab-oidc-provision.service" ];
      after = [ "homelab-oidc-provision.service" ];
    };
  };
}
