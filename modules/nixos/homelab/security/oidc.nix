# OIDC client provisioning derived from service declarations.
{ lib, config, ... }:
let
  homelabCfg = config.custom.homelab;
  cfg = homelabCfg.oidc;
  credentialsBaseDir = "/run/homelab-oidc";

  # Extract services that have OIDC enabled
  oidcServices = lib.filterAttrs (_: svc: svc.oidc.enable) homelabCfg.services;

  # Derive clients from services
  derivedClients = lib.mapAttrs (_: svc: svc.oidc // { inherit (svc.access) allowedGroups; }) oidcServices;

  # Read-only projection type for oidc.clients. Intentionally re-declares fields from schemas/oidc.nix.
  # This is a typed contract for consumers (e.g. pocket-id provisioning), not a shared schema with defaults.
  # It also adds allowedGroups which doesn't exist in schemas/oidc.nix.
  oidcClientView = lib.types.submodule {
    options = {
      enable = lib.mkOption { type = lib.types.bool; };
      name = lib.mkOption { type = lib.types.str; };
      callbackURLs = lib.mkOption { type = lib.types.listOf lib.types.str; };
      pkce = lib.mkOption { type = lib.types.bool; };
      gid = lib.mkOption { type = lib.types.nullOr lib.types.int; };
      group = lib.mkOption { type = lib.types.str; };
      credentialsDir = lib.mkOption { type = lib.types.str; };
      allowedGroups = lib.mkOption { type = lib.types.listOf lib.types.str; };
      id = {
        file = lib.mkOption { type = lib.types.str; };
        placeholder = lib.mkOption { type = lib.types.str; };
      };
      secret = {
        file = lib.mkOption { type = lib.types.str; };
        placeholder = lib.mkOption { type = lib.types.str; };
      };
      systemd = {
        dependentServices = lib.mkOption { type = lib.types.listOf lib.types.str; };
        loadCredentials = lib.mkOption { type = lib.types.listOf lib.types.str; };
        supplementaryGroups = lib.mkOption { type = lib.types.listOf lib.types.str; };
      };
    };
  };

  # Users enabled for OIDC
  enabledUsers = lib.filterAttrs (_: u: u.services.oidc.enable) homelabCfg.users;

  # Derive groups from user memberships
  allGroups = lib.unique (
    (lib.attrValues homelabCfg.groups)
    ++ lib.concatLists (lib.mapAttrsToList (_: u: u.groups) enabledUsers)
  );
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

      issuerUrl = lib.mkOption {
        type = lib.types.str;
        description = "OIDC issuer URL (e.g. https://auth.example.com)";
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

    clients = lib.mkOption {
      type = lib.types.attrsOf oidcClientView;
      default = derivedClients;
      readOnly = true;
      description = "Derived OIDC client configs keyed by service name (read-only)";
    };

    provisionConfig = lib.mkOption {
      type = lib.types.submodule {
        options = {
          users = lib.mkOption {
            type = lib.types.listOf (lib.types.submodule {
              options = {
                username = lib.mkOption { type = lib.types.str; };
                email = lib.mkOption { type = lib.types.str; };
                firstName = lib.mkOption { type = lib.types.str; };
                lastName = lib.mkOption { type = lib.types.str; };
                isAdmin = lib.mkOption { type = lib.types.bool; };
                groups = lib.mkOption { type = lib.types.listOf lib.types.str; };
              };
            });
          };
          groups = lib.mkOption {
            type = lib.types.listOf (lib.types.submodule {
              options.name = lib.mkOption { type = lib.types.str; };
            });
          };
          clients = lib.mkOption {
            type = lib.types.listOf (lib.types.submodule {
              options = {
                name = lib.mkOption { type = lib.types.str; };
                callbackURLs = lib.mkOption { type = lib.types.listOf lib.types.str; };
                pkce = lib.mkOption { type = lib.types.bool; };
                allowedGroups = lib.mkOption { type = lib.types.listOf lib.types.str; };
              };
            });
          };
        };
      };
      readOnly = true;
      default = {
        users = lib.mapAttrsToList (_: u: { inherit (u) username email firstName lastName isAdmin groups; }) enabledUsers;
        groups = map (name: { inherit name; }) allGroups;
        clients = lib.mapAttrsToList (_: client: { inherit (client) name callbackURLs pkce allowedGroups; }) derivedClients;
      };
      description = "Provisioning config derived from OIDC-enabled users and services (read-only)";
    };

    systemd = {
      baseProvisionUnit = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Systemd unit for base OIDC provisioning (users/groups)";
      };

      clientProvisionUnitPrefix = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Prefix for per-client provisioning unit names (e.g. 'pocket-id-provision-client-' yields 'pocket-id-provision-client-<name>.service')";
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (derivedClients != { }) (let
      allDependentServices = lib.concatLists (
        lib.mapAttrsToList (_: client: client.systemd.dependentServices) derivedClients
      );
      hasProvisionUnits = cfg.systemd.baseProvisionUnit != null;
      explicitGids = lib.filter (g: g != null) (lib.mapAttrsToList (_: c: c.gid) derivedClients);
      dupGids = lib.filter (gid: lib.count (g: g == gid) explicitGids > 1) (lib.unique explicitGids);
    in {
      assertions = [
        {
          assertion = dupGids == [];
          message = "OIDC clients have duplicate explicit gids: ${toString dupGids}";
        }
        {
          assertion = hasProvisionUnits || allDependentServices == [];
          message = "custom.homelab.oidc.systemd.baseProvisionUnit must be set when OIDC clients have dependentServices configured. Without it, systemd ordering is silently skipped.";
        }
      ];

      users.groups = lib.mapAttrs' (_: client:
        lib.nameValuePair client.group (lib.optionalAttrs (client.gid != null) {
          inherit (client) gid;
        })
      ) derivedClients;

      # Wire each client's dependent services to that client's provision unit
      systemd.services = lib.mkIf hasProvisionUnits (lib.mkMerge (
        lib.mapAttrsToList (name: client: let
          clientProvisionUnit = "${cfg.systemd.clientProvisionUnitPrefix}${name}.service";
        in
          lib.listToAttrs (map (svcName: {
            name = svcName;
            value = {
              requires = [ clientProvisionUnit ];
              after = [ clientProvisionUnit ];
              partOf = [ clientProvisionUnit ];
            };
          }) client.systemd.dependentServices)
        ) derivedClients
      ));
    }))
  ];
}
