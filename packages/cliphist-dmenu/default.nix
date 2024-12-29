{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-window-dmenu";
  runtimeInputs = with pkgs; [ cliphist wl-clipboard ];
  text = lib.fileContents ./cliphist-dmenu.sh;
}