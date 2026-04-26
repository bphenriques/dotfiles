{ name, config, lib, ... }:
let
  serviceConfig = config;
  credentialsBaseDir = "/run/homelab-oidc";
  mkPlaceholder = field: "@HOMELAB_OIDC_${name}_${field}@";
in
{
  options.oidc = lib.mkOption {
    type = lib.types.submodule ({ config, ... }: {
      options = {
        enable = lib.mkEnableOption "OIDC client for this service";

        id = {
          file = lib.mkOption {
            type = lib.types.str;
            default = "${credentialsBaseDir}/${name}/id";
            readOnly = true;
            description = "Path to the file containing the client ID";
          };

          placeholder = lib.mkOption {
            type = lib.types.str;
            default = mkPlaceholder "ID";
            readOnly = true;
            description = "Placeholder for client ID (use in config files, substituted at runtime)";
          };
        };

        secret = {
          file = lib.mkOption {
            type = lib.types.str;
            default = "${credentialsBaseDir}/${name}/secret";
            readOnly = true;
            description = "Path to the file containing the client secret";
          };

          placeholder = lib.mkOption {
            type = lib.types.str;
            default = mkPlaceholder "SECRET";
            readOnly = true;
            description = "Placeholder for client secret (use in config files, substituted at runtime)";
          };
        };

        name = lib.mkOption {
          type = lib.types.str;
          default = name;
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
          default = "homelab-oidc-${name}";
          readOnly = true;
          description = "Group name for this client's credentials";
        };

        credentialsDir = lib.mkOption {
          type = lib.types.str;
          default = "${credentialsBaseDir}/${name}";
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
    });
    default = { };
    description = "OIDC client configuration for this service";
  };
}
