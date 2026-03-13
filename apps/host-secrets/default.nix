{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "host-secrets";
  runtimeInputs = [ pkgs.nix ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
