{ config, lib, pkgs, osConfig, ... }:
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./dunst.nix             # Notifications
    ./hypridle.nix          # Idle behaviour
    ./hyprlock.nix          # Lock screen
    ./niri.nix              # Window Manager
    ./screen-recorder.nix   # Screen recording
    ./quickshell            # Desktop shell (recording indicator)
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
      volume-osd.enable = true;
      brightness-osd.enable = true;
      niri-keyboard-layout.enable = true;
      file-explorer.browser = config.my.programs.terminal.execApp { title = "yazi-tui"; cmd = lib.getExe config.programs.yazi.package; };
      status-glance.enable = true;
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
