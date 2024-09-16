{ config, pkgs, lib, headless, ... }:
{
  home.packages = with pkgs; lib.optionals (pkgs.stdenv.isLinux && !headless) [
    qbittorrent
    filezilla
  ];
}