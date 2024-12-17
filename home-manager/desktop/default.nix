{ pkgs, lib, config, ... }:
{
  imports = [
    ./firefox     # Browser
    ./zathura.nix # Documents
    ./mpv.nix     # Videos
    ./imv.nix     # Images
    ./logseq      # Notes
    ./beets.nix   # Music library manager
    ./discord.nix # Social
    ./ghostty.nix # Terminal applicaton
  ];

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.baobab        # Disk space analyzer
    pkgs.qbittorrent   # Torrent client
    pkgs.filezilla     # Access files remotely
    pkgs.newsflash     # RSS Reader
    pkgs.feishin       # Jellyfin player
    pkgs.cmus          # TUI music player
    pkgs.jetbrains.idea-community
  ];

  # Gaming
  xdg.mimeApps.defaultApplications."x-scheme-handler/heroic" = [ "heroic.desktop" ];

  # TODO
  custom.xdgDefaultApps = {
    archive = lib.mkBefore [ "org.kde.ark.desktop" ];
    fileBrowser = lib.mkBefore [ "org.gnome.baobab.desktop" ];
  };
}
