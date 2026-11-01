{ lib, ... }:
{
  options.fleet.shares = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          personal = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Whether this share belongs to one person rather than the household.";
          };

          root = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Absolute local root.";
          };

          backup = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Whether to include this share in the off-site backup.";
          };
        };
      }
    );
    default = { };
    description = "NAS shares available to this host.";
  };
}
