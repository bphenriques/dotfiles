{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "niri-keyboard-layout";
  runtimeInputs = [
    pkgs.niri
    pkgs.jq
    pkgs.libnotify
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}