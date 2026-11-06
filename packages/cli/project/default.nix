{ lib, pkgs, preview, builders, ... }:
builders.mkZshPlugin {
  drv = pkgs.writeShellApplication {
    name = "project";
    runtimeInputs = [ pkgs.fzf pkgs.fd preview ];
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  zshPlugin = ./plugin.zsh;
}
