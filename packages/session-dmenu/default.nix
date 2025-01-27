{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "session-dmenu";
  runtimeInputs = with pkgs; [ fuzzel niri hyprlock ];
  text = lib.fileContents ./script.sh;
}