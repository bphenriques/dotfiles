{ lib, config, ... }:


# Check custom scripts: https://github.com/iynaix/dotfiles/blob/fa261818c04e6b1aa7d928a10abd66e2c31c0ed9/packages/dotfiles-rs/dotfiles/src/bin/hypr-pip.rs
# https://github.com/dileep-kishore/nixos-hyprland/blob/main/home/common/optional/desktops/hyprland/config.nix
# https://github.com/JaKooLit/Hyprland-Dots/blob/main/config/hypr/configs/Keybinds.conf

# Force Quit active: https://github.com/JaKooLit/Hyprland-Dots/blob/main/config/hypr/configs/Keybinds.conf

# TODO: Alt F4 means keep closing active window until there is none. Then, show list of options.
let
  shortcuts = [
    "$mod, SPACE, exec, $menu"
  ];

  focus = [
    # Move focus with mod + arrow keys
    "$mod, left, movefocus, l"
    "$mod, right, movefocus, r"
    "$mod, up, movefocus, u"
    "$mod, down, movefocus, d"
  ];

  workspace = [
    # Switch workspaces with mod + [0-9]
    "$mod, 1, workspace, 1"
    "$mod, 2, workspace, 2"
    "$mod, 3, workspace, 3"
    "$mod, 4, workspace, 4"
    "$mod, 5, workspace, 5"
    "$mod, 6, workspace, 6"
    "$mod, 7, workspace, 7"
    "$mod, 8, workspace, 8"
    "$mod, 9, workspace, 9"
    "$mod, 0, workspace, 10"

    # Move active window to a workspace with mod + SHIFT + [0-9]
    "$mod SHIFT, 1, movetoworkspace, 1"
    "$mod SHIFT, 2, movetoworkspace, 2"
    "$mod SHIFT, 3, movetoworkspace, 3"
    "$mod SHIFT, 4, movetoworkspace, 4"
    "$mod SHIFT, 5, movetoworkspace, 5"
    "$mod SHIFT, 6, movetoworkspace, 6"
    "$mod SHIFT, 7, movetoworkspace, 7"
    "$mod SHIFT, 8, movetoworkspace, 8"
    "$mod SHIFT, 9, movetoworkspace, 9"
    "$mod SHIFT, 0, movetoworkspace, 10"

    # Example special workspace (scratchpad)
    "$mod, S, togglespecialworkspace, magic"
    "$mod SHIFT, S, movetoworkspace, special:magic"

    # Scroll through existing workspaces with mod + scroll
    "$mod, mouse_down, workspace, e+1"
    "$mod, mouse_up, workspace, e-1"

    # focus the previous / next desktop in the current monitor (DE style)
    "CTRL_ALT, Left, workspace, m-1"
    "CTRL_ALT, Right, workspace, m+1"
  ];
in
{
  wayland.windowManager.hyprland.settings = lib.mkMerge [
    #{ bind = [ "$mod, W, exec, pkill -SIGUSR1 waybar" ]; } # Toggle waybar
    {
      # See https://wiki.hyprland.org/Configuring/Keywords/
      "$mod" = "SUPER"; # Sets "Windows" key as main modifier

      # Mouse bindings
      bindm = [
        # Move/resize windows with mod + LMB/RMB and dragging
        "bindm = $mod, mouse:272, movewindow"
        "bindm = $mod, mouse:273, resizewindow"
      ];


      bind = [
        # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
        "$mod, Q, killactive,"
        "$mod, F, fullscreen, 0" # Fullscreen entire screen
        "$mod SHIFT, F, fullscreen, 0" # Fullscreen entire screen but with top bar
        "$mod CTRL, F, fullscreen, 0 2" # Instruct app to go fullscreen
        "$mod, P, pin" # Pin screen
        #"$mod, F, togglefloating,"
        "$mod_ALT, F4, exit,"   # Exit Hyprland # FIXME: Do I need it?
        "$mod, P, pseudo," # dwindle
        "$mod, J, togglesplit," # dwindle

        "ALT, Tab, cyclenext"
        "ALT_SHIFT, Tab, cyclenext, prev"
      ] ++ shortcuts ++ focus ++ workspace;

      bindel = [
        # Laptop multimedia keys for volume and LCD brightness
        ",XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
        ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ",XF86MonBrightnessUp, exec, brightnessctl s 10%+"
        ",XF86MonBrightnessDown, exec, brightnessctl s 10%-"
      ];

      # Requires playerctl
      bindl = [
       ", XF86AudioNext, exec, playerctl next"
       ", XF86AudioPause, exec, playerctl play-pause"
       ", XF86AudioPlay, exec, playerctl play-pause"
       ", XF86AudioPrev, exec, playerctl previous"
      ];
    }
  ];
}
