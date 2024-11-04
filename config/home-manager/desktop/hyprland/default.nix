{ config, lib, pkgs, self, ... }:
let
  wallpapersPkg = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };

  wallpapers = [
    "${wallpapersPkg}/share/wallpapers/mountains.png"
  ];
in

# TODO: https://github.com/ErikReider/SwayOSD
{
  imports = [
    ./settings.nix
    ./keybindings.nix
  ];

  home.packages = with pkgs; [
    hyprpaper             # TODO: it is a systemd service.. why doesnt it run?
    networkmanagerapplet
  ];

  services.hyprpaper = {
    enable = true;
    settings = {
      preload = wallpapers;
      wallpaper = wallpapers;
    };
  };

  # https://wiki.hyprland.org/Nix/Hyprland-on-Home-Manager/
  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      "monitor" = [ ",preferred,auto,auto" ];

      "$terminal" = "ghostty";
      "$fileManager" = "thunar";
      "$menu" = "wofi --show drun";
      "$browser" = "firefox";

      env = [
        "XDG_SESSION_TYPE,wayland"
        "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"

        # Nvidia
        "LIBVA_DRIVER_NAME,nvidia"
        "GBM_BACKEND,nvidia-drm"
        "__GLX_VENDOR_LIBRARY_NAME,nvidia"
      ];
    };
  };

  # notifications
  #services.mako.enable = true;
}