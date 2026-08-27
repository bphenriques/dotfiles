{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "nixos-install";
  runtimeInputs = [
    pkgs.disko            # Local installations
    pkgs.nixos-anywhere   # Remote installations
    selfPkgs.bw-session
    selfPkgs.dotfiles-secrets
    pkgs.sops             # Decrypt native-encrypted ZFS pool keys from the host sops file
    pkgs.jq
  ];
  text = lib.fileContents ./script.sh;
  meta.description = "Install NixOS on a host";
  meta.platforms = lib.platforms.all;
}
