{ lib, pkgs, config, self, osConfig, ... }:

# TODO: Tbh, I should just call this niri?
let
  cfg = config.custom.desktop-environment.compositor;

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };
in
{
  options.custom.desktop-environment.compositor = {
    window-switcher = mkAppOpt { };
    focused-output  = mkAppOpt { };
  };
}
