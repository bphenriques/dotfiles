{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; lib.optionals (pkgs.stdenv.isLinux && config.custom.dotfiles.graphicalEnvironment) [
    qbittorrent
    filezilla
  ];
}