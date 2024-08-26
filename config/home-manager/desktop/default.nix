{ config, pkgs, ... }:
{
  home.packages = lib.optionals (pkgs.stdenv.isLinux && config.custom.dotfiles.graphicalEnvironment) [
    pkgs.qbittorrent   # Access remote files
  ];
}
