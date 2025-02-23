{ lib, pkgs, config, self, osConfig, ... }:

let
  cfg = config.custom.desktop-environment.media-player;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  playerctl = lib.getExe pkgs.playerctl;
in
{
  options.custom.desktop-environment.media-player = {
    previous      = mkAppOpt "${playerctl} previous";
    next          = mkAppOpt "${playerctl} next";
    play-pause    = mkAppOpt "${playerctl} toggle-pause";
  };
}
