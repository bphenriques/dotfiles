{ lib, pkgs, builders, ... }:
builders.writeShellToolWithFishPlugin {
  name = "dotfiles";
  runtimeInputs = [ pkgs.nvd pkgs.nix-output-monitor ];
  text = lib.fileContents ./script.sh;
  fishPluginSrc = ./fish-plugin;
  meta.platforms = lib.platforms.all;
}
