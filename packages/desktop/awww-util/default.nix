{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "awww-util";
  runtimeInputs = [ pkgs.awww ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}
