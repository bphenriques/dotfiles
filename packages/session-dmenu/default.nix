{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "session-dmenu";
  runtimeInputs = with pkgs; [ fuzzel ];
  text = lib.fileContents ./session-dmenu.sh;
}