# Per-service secrets contract (options only).
# Imported by services-registry.nix, implemented by secrets.nix.
{ lib, serviceName }:
let
  secretsBaseDir = "/var/lib/homelab-secrets";
in
{ config, ... }: {
  options = {
    files = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
        options = {
          length = lib.mkOption {
            type = lib.types.int;
            default = 32;
            description = "Length of the secret in bytes";
          };

          rotatable = lib.mkOption {
            type = lib.types.bool;
            description = ''
              Whether this secret can be safely rotated (delete + restart).
              Set to false for bootstrap secrets that sync with external systems.
            '';
          };

          path = lib.mkOption {
            type = lib.types.str;
            default = "${secretsBaseDir}/${serviceName}/${name}";
            readOnly = true;
            description = "Path to the generated secret file";
          };
        };
      }));
      default = { };
      description = "Secret files to generate for this service";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "homelab-secrets-${serviceName}";
      readOnly = true;
      description = "Group name for accessing this service's secrets";
    };

    gid = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Fixed GID for the secrets group (null = auto-assign)";
    };

    secretsDir = lib.mkOption {
      type = lib.types.str;
      default = "${secretsBaseDir}/${serviceName}";
      readOnly = true;
      description = "Directory containing this service's secrets";
    };

    systemd.dependentServices = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Systemd services that depend on this service's secrets.
        Automatically wires requires/after/partOf/SupplementaryGroups.
      '';
    };

    envFile = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = { "RADARR__AUTH__APIKEY" = "api-key"; };
      description = ''
        Environment variables to generate from secrets.
        Keys are env var names, values are secret file names from `files`.
        Generated as ${secretsBaseDir}/${serviceName}/${serviceName}.env
      '';
    };

    envFilePath = lib.mkOption {
      type = lib.types.str;
      default = "${secretsBaseDir}/${serviceName}/${serviceName}.env";
      readOnly = true;
      description = "Path to the generated environment file";
    };
  };
}
