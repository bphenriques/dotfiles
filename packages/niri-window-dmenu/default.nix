{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-window-dmenu";
  runtimeInputs = with pkgs; [ niri fuzzel jq ];
  text = lib.fileContents ./src/niri-window-dmenu.sh;
}