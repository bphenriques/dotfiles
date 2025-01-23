{ pkgs, lib, config, ... }:
{
  imports = [
    ./firefox       # Browser
    ./zathura.nix   # Documents
    ./mpv.nix       # Videos
    ./imv.nix       # Images
    ./beets.nix     # Music library manager
    ./discord.nix   # Social
    ./ghostty.nix   # Terminal
    ./foot.nix      # Terminal
    ./mangohud.nix  # Game HUD
    ./retroarch.nix # Emulation
    ./heroic.nix    # Game launcher
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
    pkgs.gnome-calendar  # Calendar
 ];

  # FIXME
  custom.xdgDefaultApps = {
    archive = lib.mkBefore [ "org.kde.ark.desktop" ];
    fileBrowser = lib.mkBefore [ "org.gnome.baobab.desktop" ];
  };
}
