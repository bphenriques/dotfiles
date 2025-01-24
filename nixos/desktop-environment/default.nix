{ pkgs, lib, ... }:
{
  imports = [
    ./login-manager.nix
    ./nautilus.nix        # Ideally I wish it was terminal based but xdg-desktop-portal-termfilechooser seems too old to consider
    ./niri.nix
  ];

  environment.systemPackages = with pkgs; [
    qt5.qtwayland
    qt6.qtwayland

    libnotify           # Notifications
    wl-clipboard        # Wayland clipboard
    xwayland-satellite  # X11. See: https://github.com/YaLTeR/niri/wiki/Xwayland
  ];
}
