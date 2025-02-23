{ config, self, lib, ... }:
let
  mkAppOpt = default: lib.mkOption {
    inherit default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  volume = lib.getExe self.pkgs.volume-osd;
in
{
  options.custom.desktop-environment.audio = {
    increase    = mkAppOpt "${volume} increase";
    decrease    = mkAppOpt "${volume} decrease";
    toggle-mute = mkAppOpt "${volume} toggle-mute";
  };
}