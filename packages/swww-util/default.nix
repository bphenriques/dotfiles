{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "swww-util";
  runtimeInputs = [ pkgs.swww ];
  text = lib.fileContents ./script.sh;
}