{ lib, pkgs, preview, ... }:
pkgs.writeShellApplication {
  name = "fuzzy-fd";
  runtimeInputs = with pkgs; [ fzf fd preview ];
  text = lib.fileContents ./src/fuzzy-fd.sh;
}
