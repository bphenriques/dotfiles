{ pkgs, config, lib, self, ... }:
let
  cfg = config.custom.desktop;

  monitorOpt = with types; {
    name = lib.mkOption { type = string; };
    width = lib.mkOption { type = int; };
    height = lib.mkOption { type = int; };
    refreshRate = lib.mkOption { type = float; };
    scale = lib.mkOption { type = int; };
  }

  {
    name =  "Samsung Display Corp. 0x4188 Unknown";
    description = "built-in";
    width = 2880;
    height = 1800;
    refreshRate = 120.001;
    scale = 1.5;
  }


in {
  options.custom.desktop = {
    enable = lib.mkEnableOption "desktop";
    primary = monitorOpt;
  };

  config = lib.mkIf cfg.enable {
    # Assert:

  };
}
