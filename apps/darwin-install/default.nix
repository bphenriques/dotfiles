{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "darwin-install";
  runtimeInputs = [ ];
  text = lib.fileContents ./script.sh;
  meta.description = "Install nix-darwin on a host";
  meta.platforms = lib.platforms.darwin;
}