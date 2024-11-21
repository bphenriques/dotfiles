{ inputs, config, lib, pkgs, self, ... }:
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

# Screenshot: https://github.com/iynaix/dotfiles/blob/f0f8918caed8f4c245fa82fc505ae0de09a32f5c/home-manager/hyprland/screenshot.sh
let
  openOnWorkspace = workspace: program: "[workspace ${toString workspace} silent] ${program}";
in
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

  home.shellAliases = {
    hypr-log = "hyprctl rollinglog --follow";
  };

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

    #settings.bind = [
    #  "SUPER, grave, hyprexpo:expo, toggle"
    #];

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

      misc = {
        disable_autoreload = false; # disable auto polling for config file changes
        animate_mouse_windowdragging = false; # disable dragging animation
        force_default_wallpaper = -1; # Set to 0 or 1 to disable the anime mascot wallpapers
        disable_hyprland_logo = false; # If true disables the random hyprland logo / anime girl background. :(
      };

      input = {
        # Input - More on https://wiki.archlinux.org/title/Xorg/Keyboard_configuration
        kb_layout = "us,pt";
        kb_variant = "euro,";
        kb_options = builtins.concatStringsSep " " [
          "caps:ctrl_modifier"      # Replace caps-lock for Ctrl
          "grp:ralt_rshift_toggle"  # Right Alt + Right Shift: Switch keyboard layouts. See more using `xkeyboard-config`
        ];

        follow_mouse = 1;

        sensitivity = 0; # -1.0 - 1.0, 0 means no modification.

        touchpad = {
          natural_scroll = false;
          tap_button_map = "lmr";
        };
      };

      # touchpad gestures
      gestures = {
        workspace_swipe = true;
        workspace_swipe_forever = true;
      };

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