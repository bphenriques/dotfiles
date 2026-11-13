{ config, lib, pkgs, osConfig, ... }:
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./dunst.nix             # Notifications
    ./bluetooth.nix         # Bluetooth menu entry
    ./hypridle.nix          # Idle behaviour
    ./hyprlock.nix          # Lock screen
    ./niri.nix              # Window Manager
    ./screen-recorder.nix   # Screen recording
    ./dictation.nix         # Voice dictation (Voxtype)
    ./desktop-shell.nix     # Menu and recording indicator (Quickshell)
    ./calendar.nix          # Calendar menu entry and its events (desktop-shell-calendar-sync)
    ./awww.nix              # Wallpaper daemon
    ./xdg-userdirs.nix      # XDG user directories, screenshots, recordings
    ./gtk.nix               # Setup theming for some set of apps
    ./qt.nix                # Setup theming for some set of apps
    ./firefox               # Browser
    ./zathura.nix           # Documents
    ./mpv.nix               # Videos
    ./imv.nix               # Images
    ./discord.nix           # Social
    ./kdenlive.nix          # Video editor
    ./qbittorrent.nix       # Torrent client
  ];

  config = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    xdg.mimeApps.enable = true;   # Default apps and directories

    home.packages = [
      pkgs.xdg-user-dirs  # Relevant for desktop
      pkgs.gparted        # GUI partition editor
    ];

    my.programs = {
      screenshot.enable = true;
      session.enable = true;
      audioctl.enable = true;
      file-explorer.browser = config.my.programs.terminal.execApp { title = "yazi-tui"; cmd = lib.getExe config.programs.yazi.package; };
    };

    programs.satty = {
      enable = true;
      settings.general = {
        fullscreen = true;
        early-exit = true;
        disable-notifications = false;
        initial-tool = "brush";
        copy-command = "wl-copy";
        save-after-copy = false;
        output-filename = with config.my.programs.screenshot; "${directory}/${format}";
      };
    };

    my.services = {
      upower-notify = { inherit (osConfig.services.upower) enable percentageLow percentageCritical; };
    };
  };
}
