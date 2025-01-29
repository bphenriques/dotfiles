{ pkgs, lib, ... }:
{
  imports = [
    ./login-manager.nix
    ./nautilus.nix        # Ideally I wish it was terminal based but xdg-desktop-portal-termfilechooser seems too old to consider
    ./niri.nix
  ];

  environment.systemPackages = [
    pkgs.qt5.qtwayland
    pkgs.qt6.qtwayland

    pkgs.libnotify           # Notifications
    pkgs.wl-clipboard        # Wayland clipboard
    pkgs.xwayland-satellite  # X11. See: https://github.com/YaLTeR/niri/wiki/Xwayland
  ];
}
