{ lib, pkgs, builders, ... }:
builders.writeShellToolWithFishPlugin {
  name = "fzf-rg";
  runtimeInputs = [ pkgs.ripgrep pkgs.fzf ];
  text = lib.fileContents ./script.sh;
  fishPluginSrc = ./fish-plugin;
  meta.platforms = lib.platforms.all;
}
