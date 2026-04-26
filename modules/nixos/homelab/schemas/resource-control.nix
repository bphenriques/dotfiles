{ lib, homelabCfg, ... }:
let
  sliceNames = lib.attrNames homelabCfg.resourceControl.slices;
in
{
  options.resourceControl = {
    slice = lib.mkOption {
      type = lib.types.nullOr (lib.types.enum sliceNames);
      default = null;
      description = "Shared systemd slice for this service's declared systemdServices.";
    };

    systemdServices = lib.mkOption {
      type = lib.types.coercedTo lib.types.str (s: [ s ]) (lib.types.listOf lib.types.str);
      default = [ ];
      description = "Systemd service names assigned to the chosen slice. Required when slice is set.";
    };
  };
}
