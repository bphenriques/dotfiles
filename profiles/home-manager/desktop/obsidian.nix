{ lib, pkgs, config, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = with pkgs; [ obsidian ];
}