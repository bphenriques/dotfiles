{ pkgs, lib, network-devices, ... }:
{
  imports = [
    ./login-manager.nix
  ];

  programs.niri.enable = true;
  security.pam.services.swaylock = {};
  services.gnome.gnome-keyring.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gnome # Required for screencasting
    ];
  };

  # Other
  # TODO: https://github.com/Aylur/dotfiles/blob/main/nixos/system.nix#L44
  environment.systemPackages = with pkgs; [
    # Core - Dependencies
    qt5.qtwayland
    qt6.qtwayland
    libnotify

    xwayland-satellite  # X11. See: https://github.com/YaLTeR/niri/wiki/Xwayland
    konsole   # Backup terminal in case something goes wrong
    ark       # KDE package: Manage compressed files

    # Personalization
    morewaita-icon-theme
    adwaita-icon-theme
    qogir-icon-theme
    gnome-calendar
    gnome-system-monitor
    gnome-calculator      # Replace with launcher
  ];
}
