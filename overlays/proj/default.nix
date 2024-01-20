{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "projf";
  runtimeInputs = with pkgs; [ fd fzf preview ];
  text = lib.fileContents ./projf.sh;
}
