{ config, lib, pkgs, self, ... }:
{
  imports = [
    ./niri.nix      # Window Manager
    ./waybar        # Status bar
    ./mako.nix      # Notification Daemon
    ./fuzzel.nix    # Application Launcher
    ./rofi          # Application Launcher
    ./swayidle.nix  # Locks/suspends the computer when idle
    ./hyprlock.nix  # Lock screend
    ./osd.nix       # On Screen Display
    ./swww.nix      # Manage wallpapers
  ];

  home.packages = [
    (pkgs.writeScriptBin "pbcopy" (lib.getExe' pkgs.wl-clipboard "wl-copy"))      # I am too hardwired to pbcopy
    (pkgs.writeScriptBin "pbpaste" (lib.getExe' pkgs.wl-clipboard "wl-paste"))    # I am too hardwired to pbpaste
  ];

  custom.services.xwayland-satellite.enable = true;

  qt.enable = true;
  stylix.targets.qt.enable = true;
  gtk = {
    enable = true;
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc"; # Leave my $HOME
    gtk3.extraConfig = {
      gtk-error-bell = 0;
      gtk-application-prefer-dark-theme = if (config.stylix.polarity == "dark") then 1 else 0;
    };
    gtk4.extraConfig = {
      gtk-error-bell = 0;
      gtk-application-prefer-dark-theme = if (config.stylix.polarity == "dark") then 1 else 0;
    };
  };
  stylix.targets.gtk = {
    enable = true;
    flatpakSupport.enable = true;
  };
}