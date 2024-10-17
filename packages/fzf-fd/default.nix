{ lib, pkgs, preview, ... }:
pkgs.writeShellApplication {
  name = "fzf-fd";
  runtimeInputs = with pkgs; [ fzf fd preview ];
  text = lib.fileContents ./src/fzf-fd.sh;
}
