{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "nixos-install";
  runtimeInputs = [
    pkgs.disko            # Local installations
    pkgs.nixos-anywhere   # Remote installations
    selfPkgs.bw-session
    selfPkgs.dotfiles-secrets
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
