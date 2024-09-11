{ lib, pkgs, ... }:
pkgs.fishPlugins.buildFishPlugin {
  pname = "ffd";
  src = ./src/fish-plugin;
  version = "latest";
}