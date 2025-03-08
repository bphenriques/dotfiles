{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "darwin-install";
  runtimeInputs = [ ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.darwin;
}