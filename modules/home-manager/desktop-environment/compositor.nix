{ lib, pkgs, config, self, osConfig, ... }:

# TODO: Tbh, I should just call this niri?
let
  cfg = config.custom.desktop-environment.compositor;

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  displayOutputOpt = lib.types.submodule {
    options = {
      identifier  = lib.mkOption { type = lib.types.str; };
      resolution  = lib.mkOption { type = lib.types.str; };
      refreshRate = lib.mkOption { type = lib.types.str; };
      scale       = lib.mkOption { type = lib.types.str; };
    };
  };
in
{
  options.custom.desktop-environment.compositor = {
    display = {
      default = lib.mkOption {
        description = "Default display output";
        type = displayOutputOpt;
      };
    };

    window-switcher     = mkAppOpt { };
    focused-output      = mkAppOpt { };
    power-off-monitors  = mkAppOpt { };
    power-on-monitors   = mkAppOpt { };
  };

  config = {
    home.packages = [
      pkgs.wdisplays  # Ephemeral output manager
    ];
  };
}
