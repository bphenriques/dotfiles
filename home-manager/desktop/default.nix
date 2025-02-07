{ pkgs, lib, ... }:
{
  imports = [
    ./firefox           # Browser
    ./zathura.nix       # Documents
    ./mpv.nix           # Videos
    ./imv.nix           # Images
    ./beets.nix         # Music library manager
    ./foot.nix          # Terminal
    ./ghostty.nix       # Terminal
    ./mangohud.nix      # Game HUD
    ./retroarch.nix     # Emulation
    ./discord.nix       # Social
    ./heroic.nix        # Unified game client
    ./umu-launcher.nix  # Ad-hoc game launcher
  ];

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    # Internet
    pkgs.qbittorrent   # Torrent client
    pkgs.filezilla     # Access files remotely

    # Temporary
    pkgs.jetbrains.idea-community

    # Media
    pkgs.newsflash       # RSS Reader
    pkgs.feishin         # Jellyfin player
    pkgs.cmus            # TUI music player

    # System
    pkgs.wdisplays
 ];
}
