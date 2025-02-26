{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.desktop-environment.display;

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
  options.custom.desktop-environment.display = {
    default = lib.mkOption {
      description = "Default display output";
      type = displayOutputOpt;
    };
  };

  config = {
    home.packages = [ pkgs.wdisplays ];
  };
}
