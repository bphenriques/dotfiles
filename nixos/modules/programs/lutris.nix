{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.programs.lutris;
in
{
  options.modules.programs.lutris = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = mdDoc ''Whether to enable lutris.'';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      lutris
      kdialog # Required otherwise installation might not work. Note: may make only make sense for KDE?
    ];
  };
}
