{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "awww-util";
  runtimeInputs = [ pkgs.awww pkgs.findutils pkgs.coreutils pkgs.gnugrep pkgs.jq ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}
