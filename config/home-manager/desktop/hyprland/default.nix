{ config, lib, pkgs, self, ... }:
let
  wallpapersPkg = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };
in

# https://github.com/Remedan/dotfiles/blob/9c0007afb4b2c7b2dd419db598ef58a44cb92e8b/modules/user/hyprland.nix#L347 ?
# TODO: https://github.com/ErikReider/SwayOSD
# TODO: https://github.com/JaKooLit/Ja-ZaneyOS/blob/0bed326404ad90ca6803c0a9096426a36a14a35a/config/hyprland.nix#L83
# TODO: https://github.com/diniamo/niqs/blob/53288d72902365ee8d3bfdd6aff0ec79eb7c1c36/modules/workstation/hyprland.nix
# https://github.com/JaKooLit/Ja-ZaneyOS/blob/0bed326404ad90ca6803c0a9096426a36a14a35a/config/hyprland.nix
# https://github.com/Serpentian/AlfheimOS/blob/master/user/wm/hyprland/settings.nix
{
  imports = [
    ./settings.nix
    ./keybindings.nix
    ./rules.nix
  ];

  home.packages = with pkgs; [
    hyprpaper
    networkmanagerapplet
  ];

  services.hyprpaper = {
    enable = true;
    settings = {
      preload = [ "${wallpapersPkg}/share/wallpapers/mountains.png" ];
      wallpaper = [ ",${wallpapersPkg}/share/wallpapers/mountains.png" ];
    };
  };

  # https://wiki.hyprland.org/Nix/Hyprland-on-Home-Manager/
  wayland.windowManager.hyprland = {
    enable = true;
    systemd = {
      enable = true;
      variables = [ "--all" ];
      enableXdgAutostart = true;
    };
    settings = {
      "monitor" = [ ",preferred,auto,auto" ];

      "$terminal" = "konsole";
      "$fileManager" = "thunar";
      "$menu" = "fuzzel";
      "$browser" = "firefox";

      env = [
        "XDG_SESSION_TYPE,wayland"
        "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"

        # Specific for my laptop with dual gpu. lspci | grep -E 'VGA|3D' -> ls -l /dev/dri/by-path shows that card1 is the iGPU which will take precedence.
        "AQ_DRM_DEVICES,/dev/dri/card1:/dev/dri/card0"

        # Nvidia
        "LIBVA_DRIVER_NAME,nvidia"
        "GBM_BACKEND,nvidia-drm"
        "__GLX_VENDOR_LIBRARY_NAME,nvidia"
      ];

      exec-once = [
        "ags -b hypr"
        #"dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP" # Fix long-time to start systemd: https://wiki.hyprland.org/FAQ/#some-of-my-apps-take-a-really-long-time-to-open
        #"waybar"
        "${lib.getExe pkgs.hyprpaper}"
        "${lib.getExe pkgs.udiskie} --tray"
        "walker --gapplication-service"
        "${pkgs.blueman}/bin/blueman-applet"
        #"${pkgs.networkmanagerapplet}/bin/nm-applet --indicator"
      ];
    };
  };
}