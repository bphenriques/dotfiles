{ lib, pkgs, builders, ... }:
builders.mkZshPlugin {
  drv = pkgs.writeShellApplication {
    name = "fzf-rg";
    runtimeInputs = [ pkgs.ripgrep pkgs.fzf pkgs.bat ];
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  zshPlugin = ./plugin.zsh;
}
