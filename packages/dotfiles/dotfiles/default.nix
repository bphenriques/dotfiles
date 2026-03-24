{ lib, pkgs, builders, ... }:
builders.mkFishShellPlugin {
  drv = pkgs.writeShellApplication {
    name = "dotfiles";
    runtimeInputs = [ pkgs.nvd pkgs.nix-output-monitor ];
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  fishPluginSrc = ./fish-plugin;
}
