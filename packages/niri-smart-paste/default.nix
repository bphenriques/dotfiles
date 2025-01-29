{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-smart-paste";
  runtimeInputs = [ pkgs.wtype pkgs.niri ];
  text = lib.fileContents ./script.sh;
}
