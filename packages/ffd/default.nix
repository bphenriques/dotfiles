{ lib, pkgs, preview, ... }:
pkgs.writeShellApplication {
  name = "ffd-print";
  runtimeInputs = with pkgs; [ fzf fd preview ];
  text = lib.fileContents ./src/ffd.sh;
}
