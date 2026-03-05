# Per-service OIDC client contract (options only).
# Imported by services-registry.nix, implemented by oidc.nix.
{ lib, serviceName, serviceConfig }:
let
  credentialsBaseDir = "/run/homelab-oidc";
in
{ config, ... }: {
  options = {
    enable = lib.mkEnableOption "OIDC client for this service";

    name = lib.mkOption {
      type = lib.types.str;
      default = serviceName;
      description = "Name of the OIDC client in Pocket-ID";
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

    idFile = lib.mkOption {
      type = lib.types.str;
      default = "${credentialsBaseDir}/${serviceName}/id";
      readOnly = true;
      description = "Path to the file containing the client ID";
    };

    secretFile = lib.mkOption {
      type = lib.types.str;
      default = "${credentialsBaseDir}/${serviceName}/secret";
      readOnly = true;
      description = "Path to the file containing the client secret";
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
          "oidc-id:${config.idFile}"
          "oidc-secret:${config.secretFile}"
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
}
