{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "fzf-rg";
  runtimeInputs = with pkgs; [ ripgrep fzf ];
  text = lib.fileContents ./script.sh;
}
