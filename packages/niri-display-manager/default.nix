{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-display-layout";
  runtimeInputs = with pkgs; [ niri libnotify ];
  text = lib.fileContents ./src/niri-display-layout.sh;
}