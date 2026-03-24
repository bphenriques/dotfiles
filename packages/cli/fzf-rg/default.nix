{ lib, pkgs, builders, ... }:
builders.mkFishShellPlugin {
  drv = pkgs.writeShellApplication {
    name = "fzf-rg";
    runtimeInputs = [ pkgs.ripgrep pkgs.fzf ];
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  fishPluginSrc = ./fish-plugin;
}
