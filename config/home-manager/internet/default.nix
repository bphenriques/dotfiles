{ config, pkgs, lib, ... }:
{
  imports = [
    ./firefox
  ];

  home.packages = with pkgs; lib.optionals (pkgs.stdenv.isLinux) [
    qbittorrent   # Torrent client
    filezilla     # Access files remotely
    newsflash     # RSS Reader
    vesktop       # Lightweight discord
  ];

  xdg.mimeApps.defaultApplications."x-scheme-handler/discord" = [ "vesktop.desktop" ];
}