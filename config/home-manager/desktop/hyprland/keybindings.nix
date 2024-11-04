{ lib, config, ... }:

let
  shortcuts = [
    "$mod, Q, exec, $terminal"
    "$mod, E, exec, $fileManager"
    "$mod, R, exec, $menu"
    "$mod, F, exec, $browser"
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
  ];

  toggle_waybar = {
    bind = [ "$mod, W, exec, pkill -SIGUSR1 waybar" ];
    bindr = [ "$mod, W, exec, pkill -SIGUSR1 waybar" ];
  };
in
{
  wayland.windowManager.hyprland.settings = lib.mkMerge [

    {
      bind = [ "$mod, W, exec, pkill -SIGUSR1 waybar" ]; # Toggle waybar
    }
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
        "$mod, C, killactive,"
        "$mod, F, fullscreen,"
        "$mod, M, exit,"
        "$mod, V, togglefloating,"
        "$mod, P, pseudo," # dwindle
        "$mod, J, togglesplit," # dwindle

        "Ctrl+Alt, W, exec, killall waybar || waybar" # toggle waybar
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
