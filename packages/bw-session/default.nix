{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "bw-session";
  runtimeInputs = [ pkgs.bitwarden-cli pkgs.jq ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
