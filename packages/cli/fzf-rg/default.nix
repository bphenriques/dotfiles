{ lib, pkgs, builders, ... }:
builders.mkShellPlugin {
  drv = pkgs.writeShellApplication {
    name = "fzf-rg";
    runtimeInputs = [ pkgs.ripgrep pkgs.fzf pkgs.bat ];
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  fishPluginSrc = ./fish-plugin;
  zshPlugin = ./plugin.zsh;
}
