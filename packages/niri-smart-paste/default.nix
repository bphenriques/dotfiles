{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-smart-paste";
  runtimeInputs = with pkgs; [ wtype niri ];
  text = lib.fileContents ./script.sh;
}
