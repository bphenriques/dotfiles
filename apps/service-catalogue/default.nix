{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "service-catalogue";
  runtimeInputs = [ pkgs.nix pkgs.jq ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
