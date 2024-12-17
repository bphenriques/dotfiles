{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "swww-util";
  runtimeInputs = with pkgs; [ swww ];
  text = lib.fileContents ./swww-util.sh;
}