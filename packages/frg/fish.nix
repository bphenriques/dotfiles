{ lib, pkgs, ... }:
pkgs.fishPlugins.buildFishPlugin {
  pname = "frg";
  src = ./src/fish-plugin;
  version = "latest";
}