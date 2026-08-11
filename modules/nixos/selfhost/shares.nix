# NAS shares available to this host. `root` is bound by whoever owns the share here: the SMB mount
# today, a dataset mountpoint on the NAS. Callers interpolate against it, as with `xdg.configHome`.
{ lib, ... }:
{
  options.custom.shares = lib.mkOption {
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
            description = "Absolute local root. Null means this host does not have the share.";
          };

          backup = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Whether to bind this share into the off-site backup. Opt-in, because the cost is per byte stored.";
          };
        };
      }
    );
    default = { };
    description = "NAS shares available to this host.";
  };
}
