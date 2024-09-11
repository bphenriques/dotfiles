{ lib, pkgs, ... }:
pkgs.fishPlugins.buildFishPlugin {
  pname = "dotfiles";
  src = ./src/fish-plugin;
  version = "latest";
}