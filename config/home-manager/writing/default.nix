{ config, pkgs, lib, headless, ... }:
{
  imports = [
    ./helix
  ];

  home.packages = with pkgs; lib.optionals (pkgs.stdenv.isLinux && !headless) [
    qbittorrent
    filezilla
  ];
}