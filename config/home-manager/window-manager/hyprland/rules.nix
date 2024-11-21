{ lib, config, ... }:
{
  wayland.windowManager.hyprland.settings = lib.mkMerge [
    {
      windowrulev2 = [
        "dimaround,floating:1"
        "bordersize 5,fullscreen:1" # monocle mode
        "float,class:(wlroots)" # hyprland debug session


        # Ignore maximize requests from apps. You'll probably like this.
        "suppressevent maximize, class:.*"
        # "suppressevent fullscreen, class:.*"

        # Save dialog
        "float,class:(xdg-desktop-portal-gtk)"
        "size <50% <50%,class:(xdg-desktop-portal-gtk)"

        # Firefox
        "float,title:^(About Mozilla Firefox)$"
        "float,class:^(firefox)$,title:^(Picture-in-Picture)$"
        "keepaspectratio,class:^(firefox)$,title:^(Picture-in-Picture)$"

        # Misc
        "float,class:^(org.pulseaudio.pavucontrol)$"
        "float,class:^(nm-connection-editor)$"
        "float,class:^(nblueman-manager)$"
      ];
    }
    {
      layerrule = [
      ];
    }
  ];
}

#"blur,notifications"
#"ignorezero,notifications"
#"blur,swaync-notification-window"
#"ignorezero,swaync-notification-window"
#"blur,swaync-control-center"
#"ignorezero,swaync-control-center"
#"blur,logout_dialog"
