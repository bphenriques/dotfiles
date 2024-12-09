{ config, lib, pkgs, ... }:
# TODO: pre-script for gaming: https://github.com/diniamo/niqs/blob/53288d72902365ee8d3bfdd6aff0ec79eb7c1c36/modules/workstation/gaming.nix#L16

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
    ./niri.nix    # Window Manager
    ./waybar      # Top-bar. There are fancier solutions out-there.
    ./dunst.nix   # Notification Daemon
    ./kanshi.nix  # Manage external monitors
    ./fuzzel.nix  # Application Launcher
  ];

  custom.services.swww.enable = true;

  # Use the following theme: https://github.com/iynaix/dotfiles/blob/56d2d63b3b5f4c621429d79fb2aef8d44fdc25b9/home-manager/gui/gtk.nix#L85
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

  qt = {
    enable = true;
    style.name = "adwaita-dark";
    style.package = pkgs.adwaita-qt;
    platformTheme.name = "adwaita-dark";
  };
}
