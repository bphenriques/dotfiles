{ lib, pkgs, preview, builders, ... }:
builders.mkFishShellPlugin {
  drv = pkgs.writeShellApplication {
    name = "project";
    runtimeInputs = [ pkgs.fzf pkgs.fd preview ];
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  fishPluginSrc = ./fish-plugin;
}
