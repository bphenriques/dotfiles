{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "bw-session";
  runtimeInputs = [ pkgs.bitwarden-cli ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}