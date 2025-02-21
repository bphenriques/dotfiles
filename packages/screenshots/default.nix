{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "screenshot";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.grim
    pkgs.swappy
    pkgs.slurp
    pkgs.libnotify
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}