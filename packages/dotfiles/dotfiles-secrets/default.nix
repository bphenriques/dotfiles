{ lib, pkgs, bw-session, ... }:
pkgs.writeShellApplication {
  name = "dotfiles-secrets";
  runtimeInputs = [ pkgs.bitwarden-cli bw-session pkgs.jq pkgs.age pkgs.openssl pkgs.gnupg ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
