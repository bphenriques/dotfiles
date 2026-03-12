# OIDC provider configuration and client provisioning.
# Global provider config + derives clients from services.*.oidc.
#
# Client secrets are regenerated on every provision run (boot or config change).
# This ensures credentials stay fresh and avoids drift. All OIDC-consuming services
# must be listed in oidc.systemd.dependentServices to receive updated credentials.
{ lib, config, ... }:
let
  homelabCfg = config.custom.homelab;
  cfg = homelabCfg.oidc;
  credentialsBaseDir = "/run/homelab-oidc";

  # Extract services that have OIDC enabled
  oidcServices = lib.filterAttrs (_: svc: svc.oidc.enable) homelabCfg.services;

  # Derive clients from services
  derivedClients = lib.mapAttrs (_: svc: svc.oidc) oidcServices;

  # Users enabled for Pocket-ID
  enabledUsers = homelabCfg.enabledUsers.pocket-id;

  # Derive groups from user memberships
  allGroups = lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.groups) enabledUsers));
in
{
  options.custom.homelab.oidc = {
    provider = {
      displayName = lib.mkOption {
        type = lib.types.str;
        description = "Display name of the OIDC provider (shown in UI)";
      };

      internalName = lib.mkOption {
        type = lib.types.str;
        description = "Internal name for URLs and identifiers";
      };

      url = lib.mkOption {
        type = lib.types.str;
        description = "Public URL of the OIDC provider";
      };

      internalUrl = lib.mkOption {
        type = lib.types.str;
        description = "Internal URL of the OIDC provider";
      };

      discoveryEndpoint = lib.mkOption {
        type = lib.types.str;
        description = "OIDC discovery endpoint URL";
      };

      apiKeyFile = lib.mkOption {
        type = lib.types.str;
        description = "Path to file containing provider API key";
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

    # Read-only view of derived clients (from services.*.oidc).
    # Structure follows _oidc-schema.nix; validation happens at service level in services-registry.nix.
    clients = lib.mkOption {
      type = lib.types.attrsOf lib.types.attrs;
      default = derivedClients;
      readOnly = true;
      description = "Derived OIDC clients from services.*.oidc (read-only)";
    };

    # Config data for the provisioning service
    provisionConfig = lib.mkOption {
      type = lib.types.attrs;
      readOnly = true;
      default = {
        users = lib.mapAttrsToList (_: u: { inherit (u) username email firstName lastName isAdmin groups; }) enabledUsers;
        groups = map (name: { inherit name; }) allGroups;
        clients = lib.mapAttrsToList (_: client: { inherit (client) name callbackURLs pkce; }) derivedClients;
      };
      description = "Provisioning config data for Pocket-ID (users, groups, clients)";
    };

    systemd.provisionedTarget = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Systemd unit indicating OIDC credentials are ready";
    };
  };

  config = lib.mkIf (derivedClients != { }) (let
    provisionedTarget = cfg.systemd.provisionedTarget;
    allDependentServices = lib.concatLists (
      lib.mapAttrsToList (_: client: client.systemd.dependentServices) derivedClients
    );
    # Collision detection for explicit GIDs
    explicitGids = lib.filter (g: g != null) (lib.mapAttrsToList (_: c: c.gid) derivedClients);
    dupGids = lib.filter (gid: lib.count (g: g == gid) explicitGids > 1) (lib.unique explicitGids);
  in {
    assertions = [
      {
        assertion = dupGids == [];
        message = "OIDC clients have duplicate explicit gids: ${toString dupGids}";
      }
      {
        assertion = provisionedTarget != null || allDependentServices == [];
        message = "custom.homelab.oidc.systemd.provisionedTarget must be set when OIDC clients have dependentServices configured. Without it, systemd ordering is silently skipped.";
      }
    ];

    # Create groups for each OIDC client
    users.groups = lib.mapAttrs' (_: client:
      lib.nameValuePair client.group (lib.optionalAttrs (client.gid != null) {
        gid = client.gid;
      })
    ) derivedClients;

    # Auto-wire systemd dependencies for services that depend on OIDC credentials
    systemd.services = lib.mkIf (provisionedTarget != null) (lib.mkMerge (
      lib.mapAttrsToList (_: client:
        lib.listToAttrs (map (svcName: {
          name = svcName;
          value = {
            requires = [ provisionedTarget ];
            after = [ provisionedTarget ];
            partOf = [ provisionedTarget ];
          };
        }) client.systemd.dependentServices)
      ) derivedClients
    ));
  });
}
