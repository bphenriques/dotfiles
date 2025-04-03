{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "nixos-install";
  runtimeInputs = [
    pkgs.yq-go
    pkgs.jq
    selfPkgs.bw-session
    pkgs.disko            # Local installations
    pkgs.nixos-anywhere   # Remote installations
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
