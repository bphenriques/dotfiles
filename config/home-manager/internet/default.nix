{ config, pkgs, lib, ... }:
{
  imports = [
    ./firefox
    ./discord.nix
  ];

  home.packages = with pkgs; lib.optionals (pkgs.stdenv.isLinux) [
    qbittorrent   # Torrent client
    filezilla     # Access files remotely
    newsflash     # RSS Reader
    vesktop       # Lightweight discord
  ];
}