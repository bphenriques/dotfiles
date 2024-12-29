{ config, lib, pkgs, ... }:
{
  imports = [
    ./niri.nix      # Window Manager
    ./waybar        # Status bar
    ./mako.nix      # Notification Daemon
    ./fuzzel.nix    # Application Launcher
    ./rofi.nix      # Application Launcher
    ./swayidle.nix  # Locks/suspends the computer when idle
  ];

  custom.services.swww.enable = true; # Wallpaper daemon
  home.packages = [
    pkgs.cliphist  # Wayland clipboard history
    (pkgs.writeScriptBin "pbcopy" (lib.getExe' pkgs.wl-clipboard "wl-copy"))      # I am too hardwired to pbcopy
    (pkgs.writeScriptBin "pbpaste" (lib.getExe' pkgs.wl-clipboard "wl-paste"))    # I am too hardwired to pbpaste
  ];

  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
  };

  gtk = {
    enable = true;
    theme = {
      package = pkgs.adw-gtk3;
      name = "adw-gtk3-dark";
    };

    iconTheme = {
      package = pkgs.morewaita-icon-theme;
      name = "MoreWaita";
    };

    font = {
      name = "Ubuntu Nerd Font";
      package = pkgs.nerd-fonts.ubuntu;
      size = 11;
    };

    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc"; # Leave my $HOME
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
      gtk-error-bell = 0;
    };

    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
      gtk-error-bell = 0;
    };
  };

  qt = {
    enable = true;
    style.name = "adwaita-dark";
    style.package = pkgs.adwaita-qt;
    platformTheme.name = "adwaita-dark";
  };
}
