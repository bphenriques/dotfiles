{ config, lib, pkgs, ... }:
let
  nerdfonts = pkgs.nerdfonts.override {
    fonts = [
      "Ubuntu"
      "UbuntuMono"
      "CascadiaCode"
      "FantasqueSansMono"
      "FiraCode"
      "Mononoki"
    ];
  };
in
{
  # https://github.com/prasanthrangan/hyprdots?tab=readme-ov-file
  imports = [
    ./hyprland    # Window Manager
    ./niri.nix    # Window Manager

    ./kanshi.nix  # Manage external monitors
    ./fuzzel.nix  # Application Launcher
    #./ags         # Top bar, widgets, and notifications. The whole kit.
  ];

  # Use the following theme: https://github.com/iynaix/dotfiles/blob/56d2d63b3b5f4c621429d79fb2aef8d44fdc25b9/home-manager/gui/gtk.nix#L85
  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
  };

  # KDE seems to force the replacemente of this file. TODO: remove once we move away from KDE.
  home.file.${config.gtk.gtk2.configLocation}.force = true;

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
      package = nerdfonts;
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

  # Double check what this does exactly
  qt = {
    enable = true;
    package = pkgs.adwaita-qt;
    style.name = "adwaita-dark";
    platformTheme.name = "adwaita";
  };
}
