{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "fuzzy-ripgrep";
  runtimeInputs = with pkgs; [ ripgrep fzf ];
  text = lib.fileContents ./src/fuzzy-ripgrep.sh;
}
