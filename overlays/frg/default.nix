{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "frg";
  runtimeInputs = with pkgs; [ ripgrep fzf preview ];
  text = lib.fileContents ./frg.sh;
}
