{ lib, pkgs, preview, builders, ... }:
builders.writeShellToolWithFishPlugin {
  name = "fzf-fd";
  runtimeInputs = [ pkgs.fzf pkgs.fd preview ];
  text = lib.fileContents ./script.sh;
  fishPluginSrc = ./fish-plugin;
  meta.platforms = lib.platforms.all;
}
