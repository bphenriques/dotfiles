{ config, self, lib, ... }:
let
  mkAppOpt = default: lib.mkOption {
    inherit default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  brightness = lib.getExe self.pkgs.brightness-osd;
in
{
  options.custom.desktop-environment.brightness = {
    increase = mkAppOpt "${brightness} increase";
    decrease = mkAppOpt "${brightness} decrease";
  };
}