{ pkgs, ... }:
{
  # https://github.com/sodiboo/niri-flake/blob/main/flake.nix
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/wayland/wayland-session.nix
  # https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix
  programs.niri.enable = true;
  security.pam.services.swaylock = {};
  services.gnome.gnome-keyring.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gnome # Required for screencasting
    ];
  };
  programs.nm-applet.enable = true; # Network manager applet
  programs.xwayland.enable = true;

  programs.hyprland.enable = true;  # Automates several dependencies. Home-Manager sets custom configuration.

  # Other
  # TODO: https://github.com/Aylur/dotfiles/blob/main/nixos/system.nix#L44
  environment.systemPackages = with pkgs; [
    # Core - Dependencies
    qt5.qtwayland
    qt6.qtwayland
    inotify-tools
    libnotify

    # Hardware
    brightnessctl   # Manage Brightness

    xwayland-satellite  # X11. See: https://github.com/YaLTeR/niri/wiki/Xwayland
    konsole   # Backup terminal in case something goes wrong
    ark       # KDE package: Manage compressed files

    # Personalization
    morewaita-icon-theme
    adwaita-icon-theme
    qogir-icon-theme
    gnome-calendar
    gnome-system-monitor
    gnome-calculator
  ];
}
