{ lib, pkgs, ... }:

with lib;
pkgs.writeShellApplication {
  name = "smart-paste";
  runtimeInputs = with pkgs; [
    wtype
    niri
  ];
  text = fileContents ./smart-paste.sh;
}
