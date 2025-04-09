{ pkgs, config, ... }:
{
  gtk = {
    enable = true;
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc"; # Leave my $HOME

    gtk3 = {
      bookmarks = [
        "file://${config.xdg.userDirs.documents}"
        "file://${config.xdg.userDirs.pictures}"
        "file://${config.xdg.userDirs.music}"
        "file://${config.xdg.userDirs.desktop}"
        "file://${config.xdg.userDirs.download}"
        "file://${config.home.homeDirectory}/.config"
      ];
      extraConfig = {
        gtk-error-bell = 0;
        gtk-application-prefer-dark-theme = if (config.stylix.polarity == "dark") then 1 else 0;
      };
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