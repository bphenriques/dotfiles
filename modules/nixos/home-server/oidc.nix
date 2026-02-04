# OIDC Credential Management
#
# This module uses a file-based approach for OIDC credentials to:
#
# 1. Decouple provider from clients: Client services can read credentials without
#    depending on the OIDC provider (Pocket-ID) being on the same host. This enables
#    future flexibility where clients and provider may run on different machines.
#
# 2. Handle provider limitations: Pocket-ID does not support setting custom client IDs
#    or secrets during registration. It generates them automatically. Therefore, we need
#    a way to propagate generated credentials to client services after registration.
#
# Flow:
#   1. This module creates /var/lib/homelab-oidc/{client}/ directories with placeholder files
#   2. The provisioning unit (provisionUnit) registers clients and writes actual credentials
#   3. Client services wait for the provisioning unit to complete before starting
#   4. Client services read credentials from their respective files (idFile, secretFile)
#
# Per-client groups:
#   Each client gets its own group (homelab-oidc-{name}) with a deterministic GID based on
#   a hash of the client name (base 42000 + first 4 hex chars of SHA256). This ensures:
#   - Consistent GIDs across hosts for multi-host setups
#   - Per-service isolation (only services in the group can read credentials)
#   Note: GID collisions are theoretically possible but unlikely for homelab scale.
#   Renaming clients creates orphan groups; manual cleanup may be needed.
#
# Systemd units:
#   - provisionUnit: The actual unit that provisions credentials (pocket-id-init.service
#     for local, homelab-oidc-sync.service for remote). Consumers MUST After this unit.
#   - readyUnit: A target that depends on provisionUnit. Useful for conceptual grouping.
#
# Consumer pattern:
#   systemd.services.<name> = {
#     wants = [ oidcCfg.systemd.readyUnit ];
#     after = [ oidcCfg.systemd.readyUnit oidcCfg.systemd.provisionUnit ];
#     serviceConfig.SupplementaryGroups = [ oidcClient.group ];
#   };
#
# TODO: Implement homelab-oidc-sync.service for remote provider support (rsync-based)

{ lib, config, ... }:
let
  homeServerCfg = config.custom.home-server;
  credentialsBaseDir = "/var/lib/homelab-oidc";
  credentialsPlaceholder = "not-configured";

  # Deterministic GID: base + hash offset (0-65535 range)
  # Take first 4 hex chars of SHA256 hash and convert to integer
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
        default = [ "${homeServerCfg.routes.${name}.publicUrl}/oauth2/oidc/callback" ];
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

  # Unit that provisions credentials (local: pocket-id-init, remote: sync service)
  # TODO: Add homelab-oidc-sync.service for remote provider support
  oidcProvisionUnit =
    if homeServerCfg.oidc.provider.local
    then "pocket-id-init.service"
    else "homelab-oidc-sync.service";
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

      local = lib.mkOption {
        type = lib.types.bool;
        description = "Whether the OIDC provider runs on this host. Enables systemd dependencies on pocket-id-init.";
      };

      url = lib.mkOption {
        type = lib.types.str;
        description = "Public URL of the OIDC provider";
      };

      discoveryEndpoint = lib.mkOption {
        type = lib.types.str;
        description = "OIDC discovery endpoint URL";
      };
    };

    credentials = {
      dir = lib.mkOption {
        type = lib.types.str;
        default = credentialsBaseDir;
        readOnly = true;
        description = "Base directory for OIDC credentials";
      };

      placeholder = lib.mkOption {
        type = lib.types.str;
        default = credentialsPlaceholder;
        readOnly = true;
        description = "Placeholder value written to credential files before real credentials are provisioned";
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
      readyUnit = lib.mkOption {
        type = lib.types.str;
        default = "homelab-oidc-ready.target";
        readOnly = true;
        description = "Systemd target that depends on provisionUnit. Add to Wants in consuming services.";
      };
      
      provisionUnit = lib.mkOption {
        type = lib.types.str;
        default = oidcProvisionUnit;
        readOnly = true;
        description = "Systemd unit that provisions OIDC credentials. Consumers MUST add this to After.";
      };
    };
  };

  config = lib.mkIf (homeServerCfg.oidc.clients != { }) {
    assertions = [
      {
        assertion = homeServerCfg.oidc.provider.local;
        message = "Remote OIDC provider (provider.local = false) is not yet implemented. Set provider.local = true or remove clients.";
      }
    ];

    # Per-client groups with deterministic GIDs
    users.groups = lib.mkMerge (lib.mapAttrsToList (name: _: {
      ${clientGroup name} = { gid = clientGid name; };
    }) homeServerCfg.oidc.clients);

    # Base directory + per-client subdirectories for OIDC credentials
    systemd.tmpfiles.rules = [
      "d ${credentialsBaseDir} 0750 root root -"
    ] ++ lib.flatten (lib.mapAttrsToList (_: client: [
      "d ${client.credentialsDir} 0750 root ${client.group} -"
      "f ${client.idFile} 0640 root ${client.group} - ${credentialsPlaceholder}"
      "f ${client.secretFile} 0640 root ${client.group} - ${credentialsPlaceholder}"
    ]) homeServerCfg.oidc.clients);

    # Target that indicates OIDC credentials are ready for all configured clients
    systemd.targets."homelab-oidc-ready" = {
      description = "OIDC credentials are ready";
      wants = [ oidcProvisionUnit ];
      after = [ oidcProvisionUnit ];
      requires = [ oidcProvisionUnit ];
    };
  };
}
