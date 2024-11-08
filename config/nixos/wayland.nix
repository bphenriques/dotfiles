{ pkgs, ... }:
{

  programs.hyprland.enable = true;  # Automates several dependencies. Home-Manager sets custom configuration.
  xdg.portal.enable = true;

  # Other
  programs.nm-applet.enable = true;

  # TODO: https://github.com/Aylur/dotfiles/blob/main/nixos/system.nix#L44

  environment.systemPackages = with pkgs; [
    morewaita-icon-theme
    adwaita-icon-theme
    qogir-icon-theme
    gnome-calendar
    gnome-system-monitor
    gnome-calculator

    # Review
    nautilus
    brightnessctl
    konsole # Lets ensure this is around
    ark     # I still like this?
  ];
}