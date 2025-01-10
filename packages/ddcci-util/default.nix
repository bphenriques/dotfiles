{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "ddcci-util";
  runtimeInputs = with pkgs; [ kmod ddcutil gnugrep ];
  text = lib.fileContents ./ddcci-util.sh;
}