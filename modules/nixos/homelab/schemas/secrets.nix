{ name, lib, ... }:
let
  ownerName = name;
  secretsBaseDir = "/var/lib/homelab-secrets";
  mkPlaceholder = secretName: "@HOMELAB_SECRET_${ownerName}_${secretName}@";
in
{
  options.secrets = lib.mkOption {
    type = lib.types.submodule ({ config, ... }: {
      options = {
        files = lib.mkOption {
          type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
            options = {
              bytes = lib.mkOption {
                type = lib.types.int;
                default = 32;
                description = "Number of random bytes to generate (output file will be 2x chars due to hex encoding)";
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
                default = "${secretsBaseDir}/${ownerName}/${name}";
                readOnly = true;
                description = "Path to the generated secret file";
              };
            };
          }));
          default = { };
          description = "Secret files to generate for this owner";
        };

        placeholder = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = lib.mapAttrs (fileName: _: mkPlaceholder fileName) config.files;
          readOnly = true;
          description = "Placeholder strings for each secret file. Use in template content.";
        };

        templates = lib.mkOption {
          type = lib.types.attrsOf (lib.types.submodule ({ name, ... }: {
            options = {
              content = lib.mkOption {
                type = lib.types.lines;
                description = ''
                  Template content with placeholders. Use secrets.placeholder.<name> for substitution.
                  Placeholders are replaced at runtime using replace-secret.
                '';
              };

              path = lib.mkOption {
                type = lib.types.str;
                default = "${secretsBaseDir}/${ownerName}/${name}";
                description = "Path where the rendered template will be written";
              };
            };
          }));
          default = { };
          description = "Template files derived from this owner's secrets";
        };

        group = lib.mkOption {
          type = lib.types.str;
          default = "homelab-secrets-${ownerName}";
          readOnly = true;
          description = "Group name for accessing this owner's secrets";
        };

        gid = lib.mkOption {
          type = lib.types.nullOr lib.types.int;
          default = null;
          description = "Fixed GID for the secrets group (null = auto-assign)";
        };

        secretsDir = lib.mkOption {
          type = lib.types.str;
          default = "${secretsBaseDir}/${ownerName}";
          readOnly = true;
          description = "Directory containing this owner's secrets";
        };

        systemd.dependentServices = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = ''
            Systemd services that depend on this owner's secrets.
            Automatically wires requires/after/partOf/SupplementaryGroups.
          '';
        };
      };
    });
    default = { };
    description = "Generated secrets for this service/task";
  };
}
