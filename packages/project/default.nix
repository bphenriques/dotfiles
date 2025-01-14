{ lib, pkgs, preview, ... }:
pkgs.writeShellApplication {
  name = "project";
  runtimeInputs = with pkgs; [ fzf fd preview ];
  text = lib.fileContents ./script.sh;
}
