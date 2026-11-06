{ lib, pkgs, builders, ... }:
builders.mkZshPlugin {
  drv = pkgs.writeShellApplication {
    name = "dotfiles";
    runtimeInputs = [ pkgs.nh pkgs.nvd pkgs.coreutils ];   # GNU ls: macOS ls -v is not a version sort
    text = lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  zshPlugin = ./plugin.zsh;
}
