{ lib, pkgs, preview, builders, ... }:
builders.mkShellPlugin {
  drv = pkgs.writeShellApplication {
    name = "project";
    runtimeInputs = [ pkgs.fzf pkgs.fd preview ];
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  fishPluginSrc = ./fish-plugin;
  zshPlugin = ./plugin.zsh;
}
