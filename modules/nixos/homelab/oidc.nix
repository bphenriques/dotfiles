{ lib, config, ... }:
let
  homelabCfg = config.custom.homelab;
  cfg = homelabCfg.oidc;
  credentialsBaseDir = "/run/homelab-oidc";

  # Deterministic GID: base + hash offset (0-65535 range)
  baseGid = 42000;
  hashOffset = name: lib.fromHexString (builtins.substring 0 4 (builtins.hashString "sha256" name));
  clientGid = name: baseGid + (hashOffset name);
  clientGroup = name: "homelab-oidc-${name}";

  clientOpt = lib.types.submodule ({ name, config, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Name of the OIDC client in Pocket-ID";
      };

      callbackURLs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "${homelabCfg.services.${name}.publicUrl}/oauth2/oidc/callback" ];
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

      systemd = {
        dependentServices = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = ''
            Systemd services that depend on this OIDC client's credentials being provisioned.

            For each service listed, the module automatically adds:
              - requires = [ provisionedTarget ]
              - after = [ provisionedTarget ]
              - partOf = [ provisionedTarget ]

            This ensures services don't start until OIDC credentials are ready and restart
            when the OIDC provisioning service restarts.

            Credential handling (LoadCredential, SupplementaryGroups) must still be configured
            explicitly per service, as patterns vary.

            Example:
              custom.homelab.oidc.clients.myapp.systemd.dependentServices = [ "myapp" "myapp-configure" ];
          '';
        };

        loadCredentials = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "oidc-id:${config.idFile}"
            "oidc-secret:${config.secretFile}"
          ];
          readOnly = true;
          description = ''
            Ready-to-use LoadCredential entries for systemd services.

            Use when the service supports reading credentials from files specified via environment variables.
            The credentials are loaded into systemd's secure credentials directory.

            Example:
              systemd.services.myservice.serviceConfig.LoadCredential = oidcClient.systemd.loadCredentials;
          '';
        };

        credentialPaths = {
          id = lib.mkOption {
            type = lib.types.str;
            default = "\${CREDENTIALS_DIRECTORY}/oidc-id";
            readOnly = true;
            description = ''
              Path to client ID within systemd's credentials directory.

              Use in environment variables that expect a file path (e.g., OAUTH2_CLIENT_ID_FILE).
              Requires loadCredentials to be set on the service.

              Example:
                environment.OAUTH2_CLIENT_ID_FILE = oidcClient.systemd.credentialPaths.id;
            '';
          };
          secret = lib.mkOption {
            type = lib.types.str;
            default = "\${CREDENTIALS_DIRECTORY}/oidc-secret";
            readOnly = true;
            description = ''
              Path to client secret within systemd's credentials directory.

              Use in environment variables that expect a file path (e.g., OAUTH2_CLIENT_SECRET_FILE).
              Requires loadCredentials to be set on the service.

              Example:
                environment.OAUTH2_CLIENT_SECRET_FILE = oidcClient.systemd.credentialPaths.secret;
            '';
          };
        };

        supplementaryGroups = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ config.group ];
          readOnly = true;
          description = ''
            Groups to add to service's SupplementaryGroups for direct credential file access.

            Use when the service reads credentials directly from idFile/secretFile paths
            (e.g., Nix's _secret pattern, container volume mounts) rather than via systemd credentials.

            Example:
              systemd.services.myservice.serviceConfig.SupplementaryGroups = oidcClient.systemd.supplementaryGroups;
          '';
        };
      };
    };
  });

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
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Systemd unit indicating OIDC credentials are ready. Must be set by the provisioning service.";
    };
  };

  config = lib.mkIf (cfg.clients != { }) (let
    provisionedTarget = cfg.systemd.provisionedTarget;
    allDependentServices = lib.concatLists (
      lib.mapAttrsToList (_: client: client.systemd.dependentServices) cfg.clients
    );
  in {
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
      ) cfg.clients
    ));

    warnings = lib.optional (provisionedTarget == null && allDependentServices != [ ])
      "custom.homelab.oidc.systemd.provisionedTarget is not set but OIDC clients have dependentServices configured. Services will lack correct systemd dependencies.";
  });
}
