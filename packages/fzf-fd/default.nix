{ lib, pkgs, preview, ... }:
pkgs.writeShellApplication {
  name = "fzf-fd";
  runtimeInputs = [ pkgs.fzf pkgs.fd preview ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
