{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "nixos-install";
  runtimeInputs = [
    pkgs.disko            # Local installations
    pkgs.nixos-anywhere   # Remote installations
    pkgs.parted           # SD card installations (partprobe)
    pkgs.zstd             # SD card installations (zstdcat)
    selfPkgs.bw-session
    selfPkgs.dotfiles-secrets
  ];
  text = lib.fileContents ./script.sh;
  meta.description = "Install NixOS on a host";
  meta.platforms = lib.platforms.all;
}
