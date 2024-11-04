{ config, lib, pkgs, ... }:
{
  # https://github.com/prasanthrangan/hyprdots?tab=readme-ov-file
  imports = [
    ./hyprland
    ./wofi.nix    # Application Launcher
    ./waybar      # Bar on top
    ./kanshi.nix  # Display Manager
    ./dunst.nix   # Notifications
  ];

  # Use the following theme: https://github.com/iynaix/dotfiles/blob/56d2d63b3b5f4c621429d79fb2aef8d44fdc25b9/home-manager/gui/gtk.nix#L85
  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
  };

  home.file.${config.gtk.gtk2.configLocation}.force = true;

  gtk = {
    enable = true;
    theme = {
      package = pkgs.flat-remix-gtk;
      name = "Flat-Remix-GTK-Grey-Darkest";
    };

    iconTheme = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };

    font = {
      name = "Sans";
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
}