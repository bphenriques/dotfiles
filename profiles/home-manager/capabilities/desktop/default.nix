{ pkgs, lib, ... }:
{
  imports = [
    ./stylix.nix          # Color scheme, fonts, icons, cursor
    ./xdg-userdirs.nix    # XDG user directories, screenshots, recordings
    ./btop.nix            # System Monitor
    ./fish.nix            # Shell
    ./fzf.nix             # Fuzzy search
    ./qt.nix              # Setup theming for some set of apps
    ./gtk.nix             # Setup theming for some set of apps
    ./firefox             # Browser
    ./zathura.nix         # Documents
    ./mpv.nix             # Videos
    ./imv.nix             # Images
    ./beets.nix           # Music library manager
    ./ghostty.nix         # Terminal
    ./discord.nix         # Social
    ./rofi.nix            # Alternative customizable menu
    ./kdenlive.nix        # Video editor
    ./qbittorrent.nix     # Torrent client
    ./mpd.nix             # Music player
    ./awww.nix            # Wallpaper daemon
    ./obsidian.nix        # note taking
    ./yazi.nix            # File browser
  ];

  xdg.mimeApps.enable = pkgs.stdenv.isLinux;    # Default apps and directories

  # Enable easier font management
  fonts.fontconfig.enable = true;
  stylix.targets.fontconfig.enable = true;

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.xdg-user-dirs  # Relevant for desktop
    pkgs.gparted        # GUI partition editor
  ];
}
