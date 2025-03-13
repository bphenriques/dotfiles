{ pkgs, lib, ... }:
{
  imports = [
    ./qt.nix            # Setup theming for some set of apps
    ./gtk.nix           # Setup theming for some set of apps
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
    ./rofi.nix          # Alternative customizable menu
    ./heroic.nix        # Unified game client
    ./umu-launcher.nix  # Ad-hoc game launcher
    ./mpd.nix           # Music server
    ./ncmpcpp.nix       # MPD client
  ];

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    # Internet
    pkgs.qbittorrent   # Torrent client
    pkgs.filezilla     # Access files remotely

    # Temporary
    pkgs.jetbrains.idea-community

    # Media
    pkgs.feishin         # Jellyfin player
    pkgs.cmus            # TUI music player
  ];

  custom.services.xwayland-satellite.enable = pkgs.stdenv.isLinux;
}
