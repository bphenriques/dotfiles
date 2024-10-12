{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "frg";
  runtimeInputs = with pkgs; [ ripgrep fzf ];
  text = lib.fileContents ./src/frg.sh;
}
