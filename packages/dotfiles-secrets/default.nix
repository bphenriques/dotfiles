{ lib, pkgs, bw-session, ... }:
pkgs.writeShellApplication {
  name = "dotfiles-secrets";
  runtimeInputs = [ pkgs.bitwarden-cli pkgs.jq pkgs.yq-go ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
