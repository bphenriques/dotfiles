{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-window-dmenu";
  runtimeInputs = [ pkgs.niri pkgs.fuzzel pkgs.jq ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}