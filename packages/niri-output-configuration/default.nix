{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-output-configuration";
  runtimeInputs = with pkgs; [ niri libnotify ];
  text = lib.fileContents ./src/niri-output-configuration.sh;
}