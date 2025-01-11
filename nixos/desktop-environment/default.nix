{ pkgs, lib, network-devices, ... }:
{
  imports = [
    ./login-manager.nix
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
