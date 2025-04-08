{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "post-install";
  runtimeInputs = [
    pkgs.git
    pkgs.gnupg
    pkgs.bitwarden-cli
    selfPkgs.bw-session
    selfPkgs.dotfiles-secrets
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
