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

    # System
    # TODO: cpu has to show cpufreq driver, governor and power pref
    # TODO: I like the services view
    pkgs.mission-center  # TODO: This is nice.. but can btop or variants replace it?
 ];

  # FIXME
  custom.xdgDefaultApps = {
    archive = lib.mkBefore [ "org.kde.ark.desktop" ];
    fileBrowser = lib.mkBefore [ "org.gnome.baobab.desktop" ];
  };
}
