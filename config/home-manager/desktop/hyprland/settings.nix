{ pkgs, lib, config, ... }:

let
  pointer = config.home.pointerCursor;
  cursorName = "Bibata-Modern-Classic-Hyprcursor";
in

# https://github.com/hyprwm/Hyprland/blob/main/example/hyprland.conf
# https://github.com/iynaix/dotfiles/blob/f0f8918caed8f4c245fa82fc505ae0de09a32f5c/home-manager/hyprland/default.nix
{
  #
  wayland.windowManager.hyprland.settings = {
    env = [
      "XDG_SESSION_TYPE,wayland"
      "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"

      # Nvidia
      "LIBVA_DRIVER_NAME,nvidia"
      "GBM_BACKEND,nvidia-drm"
      "__GLX_VENDOR_LIBRARY_NAME,nvidia"
    ];

    general = {
      gaps_in = 2;
      gaps_out = 0;
      border_size = 1;
      layout = "master";
      no_border_on_floating = true;

      # https://wiki.hyprland.org/Configuring/Variables/#variable-types for info about colors
      "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
      "col.inactive_border" = "rgba(595959aa)";

      resize_on_border = true;

      # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
      allow_tearing = false;
    };

    decoration = {
      rounding = 2;


      dim_inactive = true;

      drop_shadow = true;
      shadow_range = 4;
      shadow_render_power = 3;
      "col.shadow" = "rgba(1a1a1aee)";

      # https://wiki.hyprland.org/Configuring/Variables/#blur
      blur = {
        enabled = true;
        size = 2;
        passes = 3;
        new_optimizations = true;
      };

      # Disable shadows? I honstly dont care much about blur
    };

    # https://wiki.hyprland.org/Configuring/Variables/#animations
    animations = {
      enabled = true;

      # Default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
      bezier = [
        "overshot, 0.05, 0.9, 0.1, 1.05"
        "smoothOut, 0.36, 0, 0.66, -0.56"
        "smoothIn, 0.25, 1, 0.5, 1"
      ];

      animation = [
        "windows, 1, 5, overshot, slide"
        "windowsOut, 1, 4, smoothOut, slide"
        "windowsMove, 1, 4, smoothIn, slide"
        "layers, 1, 5, default, popin 80%"
        "border, 1, 5, default"
        # 1 loop every 5 minutes
        "borderangle, 1, ${toString (10 * 60 * 5)}, default, loop"
        "fade, 1, 5, smoothIn"
        "fadeDim, 1, 5, smoothIn"
        "workspaces, 1, 6, default"
      ];
    };

    # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
    dwindle = {
      pseudotile = true; # Master switch for pseudotiling. Enabling is bound to mod + P in the keybinds section below
      preserve_split = true; # You probably want this
    };

    # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
    master = {
      new_status = "master";
    };

    exec-once = [
      "hyprctl setcursor ${cursorName} ${toString pointer.size}"
    ];

    misc = {
      disable_autoreload = false; # disable auto polling for config file changes
      animate_mouse_windowdragging = false; # disable dragging animation
      force_default_wallpaper = -1; # Set to 0 or 1 to disable the anime mascot wallpapers
      disable_hyprland_logo = false; # If true disables the random hyprland logo / anime girl background. :(
    };

    input = {
      kb_layout = "us,pt";
      kb_variant = "euro,";
      kb_options = "caps:ctrl_modifier";

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
  };
}

#  # Input - More on https://wiki.archlinux.org/title/Xorg/Keyboard_configuration
#  services.xserver = {
#    #exportConfiguration = true;  # Do I need this?
#    xkb.layout = "us,pt";       # localectl list-x11-keymap-layouts and
#    xkb.variant = "euro,";      # localectl list-x11-keymap-variants us
#    xkb.options = builtins.concatStringsSep " " [
#      "caps:ctrl_modifier"      # Replace caps-lock for Ctrl
#      "grp:ralt_rshift_toggle"  # Right Alt + Right Shift: Switch keyboard layouts. See more using `xkeyboard-config`
#    ];
#
#    excludePackages = [ pkgs.xterm ];
#  };