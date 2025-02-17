{ pkgs, config, ... }:
{
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