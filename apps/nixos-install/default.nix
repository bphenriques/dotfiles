{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "nixos-install";
  runtimeInputs = [
    pkgs.yq-go
    pkgs.jq
    selfPkgs.bw-session
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}