{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "screen-recorder";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.wl-screenrec
    pkgs.slurp
    pkgs.libnotify
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}