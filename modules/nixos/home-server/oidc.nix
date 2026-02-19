# OIDC Configuration
#
# This module defines OIDC options and generates the provisioning config.
# The actual provisioning service is defined in hosts/compute/services/oidc-provision/.
#
# Consumer pattern:
#   systemd.services.<name> = {
#     wants = [ config.custom.home-server.oidc.systemd.provisionedTarget ];
#     after = [ config.custom.home-server.oidc.systemd.provisionedTarget ];
#     serviceConfig.SupplementaryGroups = [ oidcClient.group ];
#   };
#
{ lib, config, ... }:
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

      gid = lib.mkOption {
        type = lib.types.int;
        default = clientGid name;
        readOnly = true;
        description = "GID for this client's credentials group";
      };

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

  # Users enabled for Pocket-ID
  enabledUsers = homeServerCfg.enabledUsers.pocket-id;

  # Derive groups from user memberships
  allGroups = lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.groups) enabledUsers));
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

      internalUrl = lib.mkOption {
        type = lib.types.str;
        description = "Internal URL of the OIDC provider when beloging to the same network";
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

    # Config data for the provisioning service
    provisionConfig = lib.mkOption {
      type = lib.types.attrs;
      readOnly = true;
      default = {
        users = lib.mapAttrsToList (_: u: { inherit (u) username email firstName lastName isAdmin groups; }) enabledUsers;
        groups = map (name: { inherit name; }) allGroups;
        clients = lib.mapAttrsToList (_: client: { inherit (client) name callbackURLs pkce; }) cfg.clients;
      };
      description = "Provisioning config data for Pocket-ID (users, groups, clients)";
    };

    systemd.provisionedTarget = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      description = "Systemd service indicating OIDC credentials are ready. Use in both wants and after.";
    };
  };
}
