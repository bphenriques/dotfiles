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

  mkPlaceholder = serviceName: field: "@HOMELAB_OIDC_${serviceName}_${field}@";

  mkServiceOidcSchema = { serviceName, serviceConfig }: { config, ... }: {
    options = {
      enable = lib.mkEnableOption "OIDC client for this service";

      id = {
        file = lib.mkOption {
          type = lib.types.str;
          default = "${credentialsBaseDir}/${serviceName}/id";
          readOnly = true;
          description = "Path to the file containing the client ID";
        };

        placeholder = lib.mkOption {
          type = lib.types.str;
          default = mkPlaceholder serviceName "ID";
          readOnly = true;
          description = "Placeholder for client ID (use in config files, substituted at runtime)";
        };
      };

      secret = {
        file = lib.mkOption {
          type = lib.types.str;
          default = "${credentialsBaseDir}/${serviceName}/secret";
          readOnly = true;
          description = "Path to the file containing the client secret";
        };

        placeholder = lib.mkOption {
          type = lib.types.str;
          default = mkPlaceholder serviceName "SECRET";
          readOnly = true;
          description = "Placeholder for client secret (use in config files, substituted at runtime)";
        };
      };

      name = lib.mkOption {
        type = lib.types.str;
        default = serviceName;
        description = "Display name of the OIDC client in the provider";
      };

      callbackURLs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "${serviceConfig.publicUrl}/oauth2/oidc/callback" ];
        description = "Callback URLs for the OIDC client";
      };

      pkce = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable PKCE for this client";
      };

      gid = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
        description = "Fixed GID for the credentials group (null = auto-assign)";
      };

      group = lib.mkOption {
        type = lib.types.str;
        default = "homelab-oidc-${serviceName}";
        readOnly = true;
        description = "Group name for this client's credentials";
      };

      credentialsDir = lib.mkOption {
        type = lib.types.str;
        default = "${credentialsBaseDir}/${serviceName}";
        readOnly = true;
        description = "Directory containing this client's id and secret files";
      };

      systemd = {
        dependentServices = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = ''
            Systemd services that depend on this OIDC client's credentials.
            Automatically wires requires/after/partOf.
          '';
        };

        loadCredentials = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "oidc-id:${config.id.file}"
            "oidc-secret:${config.secret.file}"
          ];
          readOnly = true;
          description = "Ready-to-use LoadCredential entries for systemd services";
        };

        supplementaryGroups = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ config.group ];
          readOnly = true;
          description = "Groups to add for direct credential file access";
        };
      };
    };
  };

  # Extract services that have OIDC enabled
  oidcServices = lib.filterAttrs (_: svc: svc.oidc.enable) homelabCfg.services;

  # Derive clients from services
  derivedClients = lib.mapAttrs (_: svc: svc.oidc) oidcServices;

  # Users enabled for OIDC
  enabledUsers = lib.filterAttrs (_: u: u.services.oidc.enable) homelabCfg.users;

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

    clients = lib.mkOption {
      type = lib.types.attrsOf lib.types.attrs;
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
              };
            });
          };
        };
      };
      readOnly = true;
      default = {
        users = lib.mapAttrsToList (_: u: { inherit (u) username email firstName lastName isAdmin groups; }) enabledUsers;
        groups = map (name: { inherit name; }) allGroups;
        clients = lib.mapAttrsToList (_: client: { inherit (client) name callbackURLs pkce; }) derivedClients;
      };
      description = "Provisioning config derived from OIDC-enabled users and services (read-only)";
    };

    systemd.provisionedTarget = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Systemd unit indicating OIDC credentials are ready";
    };
  };

  config = lib.mkMerge [
    {
      custom.homelab._serviceOptionExtensions = [
        ({ name, config, ... }: {
          options.oidc = lib.mkOption {
            type = lib.types.submodule (mkServiceOidcSchema {
              serviceName = name;
              serviceConfig = config;
            });
            default = { };
            description = "OIDC client configuration for this service";
          };
        })
      ];

      custom.homelab._userOptionExtensions = [
        ({ ... }: {
          options.services.oidc.enable = lib.mkEnableOption "OIDC account for this user" // {
            default = true;
          };
        })
      ];
    }

    (lib.mkIf (derivedClients != { }) (let
      provisionedTarget = cfg.systemd.provisionedTarget;
      allDependentServices = lib.concatLists (
        lib.mapAttrsToList (_: client: client.systemd.dependentServices) derivedClients
      );
      explicitGids = lib.filter (g: g != null) (lib.mapAttrsToList (_: c: c.gid) derivedClients);
      dupGids = lib.filter (gid: lib.count (g: g == gid) explicitGids > 1) (lib.unique explicitGids);
    in {
      assertions = [
        {
          assertion = enabledUsers != { };
          message = "At least one user must be enabled for OIDC (services.oidc.enable = true)";
        }
        {
          assertion = dupGids == [];
          message = "OIDC clients have duplicate explicit gids: ${toString dupGids}";
        }
        {
          assertion = provisionedTarget != null || allDependentServices == [];
          message = "custom.homelab.oidc.systemd.provisionedTarget must be set when OIDC clients have dependentServices configured. Without it, systemd ordering is silently skipped.";
        }
      ];

      users.groups = lib.mapAttrs' (_: client:
        lib.nameValuePair client.group (lib.optionalAttrs (client.gid != null) {
          gid = client.gid;
        })
      ) derivedClients;

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
    }))
  ];
}
