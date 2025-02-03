{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "secrets";
  runtimeInputs = [ ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}