{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "post-install";
  runtimeInputs = [
    pkgs.git
    pkgs.yq-go
    pkgs.age
    pkgs.sops
    pkgs.gnupg
    selfPkgs.bw-session
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}