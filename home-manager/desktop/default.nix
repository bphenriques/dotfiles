{ pkgs, lib, config, ... }:
{
  imports = [
    ./firefox       # Browser
    ./zathura.nix   # Documents
    ./mpv.nix       # Videos
    ./imv.nix       # Images
    ./logseq        # Notes
    ./beets.nix     # Music library manager
    ./discord.nix   # Social
    ./ghostty.nix   # Terminal applicaton
    ./mangohud.nix  # Game HUD
    ./retroarch.nix # Emulation
    ./heroic.nix    # Game launcher
  ];

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    # Internet
    pkgs.qbittorrent   # Torrent client
    pkgs.filezilla     # Access files remotely

    # Media
    pkgs.newsflash     # RSS Reader
    pkgs.feishin       # Jellyfin player
    pkgs.cmus          # TUI music player

    # Development
    pkgs.jetbrains.idea-community

    # System
    pkgs.baobab        # Disk space analyzer
  ];

  # TODO
  custom.xdgDefaultApps = {
    archive = lib.mkBefore [ "org.kde.ark.desktop" ];
    fileBrowser = lib.mkBefore [ "org.gnome.baobab.desktop" ];
  };
}
