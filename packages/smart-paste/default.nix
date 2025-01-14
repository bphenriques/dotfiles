{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "smart-paste";
  runtimeInputs = with pkgs; [ wtype niri ];
  text = lib.fileContents ./script.sh;
}
