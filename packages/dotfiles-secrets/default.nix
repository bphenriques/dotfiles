{ lib, pkgs, bw-session, ... }:
pkgs.writeShellApplication {
  name = "dotfiles-secrets";
  runtimeInputs = [ pkgs.bitwarden-cli pkgs.jq pkgs.age pkgs.openssl ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
