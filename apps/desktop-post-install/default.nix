{ lib, pkgs, selfPkgs, ... }:
pkgs.writeShellApplication {
  name = "desktop-post-install";
  runtimeInputs = [
    pkgs.git
    pkgs.gnupg
    pkgs.bitwarden-cli
    selfPkgs.bw-session
    selfPkgs.dotfiles-secrets
  ];
  text = lib.fileContents ./script.sh;
  meta.description = "Post-install setup for desktop hosts";
  meta.platforms = lib.platforms.all;
}
