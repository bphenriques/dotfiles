{ pkgs, lib, ... }:
{
  imports = [
    ./qt.nix              # Setup theming for some set of apps
    ./gtk.nix             # Setup theming for some set of apps
    ./firefox             # Browser
    ./zathura.nix         # Documents
    ./mpv.nix             # Videos
    ./imv.nix             # Images
    ./beets.nix           # Music library manager
    ./foot.nix            # Terminal (very fast to open and no GPU acceleration)
    ./ghostty.nix         # Terminal (slower to open and has GPU acceleration)
    ./mangohud.nix        # Game HUD
    ./retroarch.nix       # Emulation
    ./discord.nix         # Social
    ./rofi.nix            # Alternative customizable menu
    ./heroic.nix          # Unified game client
    ./umu-launcher.nix    # Ad-hoc game launcher
    ./mpd.nix             # Music player
    ./wallpaper.nix       # Set wallpaper
  ];

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.qbittorrent   # Torrent client. FIXME: alternative TUI?

    # Rom management
    pkgs.igir-git      # Remove once https://github.com/NixOS/nixpkgs/pull/372184 is merged
    (pkgs.skyscraper.override { enableXdg = true; })     # Adds media files next to the files
 ];

  custom.services.xwayland-satellite.enable = true;
}
