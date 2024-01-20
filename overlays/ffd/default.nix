{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "ffd-print";
  runtimeInputs = with pkgs; [ fzf fd ];
  text = lib.fileContents ./ffd.sh;
}
