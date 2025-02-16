{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "session-dmenu";
  runtimeInputs = [ pkgs.fuzzel pkgs.niri pkgs.hyprlock pkgs.foot ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}