{ lib, config, pkgs, ... }:
# https://codeberg.org/explosion-mental/wallust generates colors autoamtically.
let
  alertSpan = s: ''<span color="{{color4}}">${s}</span>'';
in
{
  # https://github.com/nix-community/home-manager/issues/3599
  # do not use the systemd service as it is flaky and unreliable

  programs.waybar = {
    enable = true;
    style = ./style.css;
    settings = {
      "bar" = {
        output = ["eDP-1" "HDMI-A-1"];

        layer = "top";
        position = "top";
        margin-left = 9;
        margin-right = 9;
        margin-top = 0;
        margin-bottom = 0;
        spacing = 4;

        reload_style_on_change = true;

        modules-left = [
          "custom/spacer"
          "hyprland/workspaces"
          "hyprland/window"
        ]; # [ "custom/nix" "idle_inhibitor" ];
        modules-center = [
        ];

        modules-right = [
          "pulseaudio"
          "network"
          "memory"
          "cpu"
          "backlight"
          "battery"
          "clock"
          "tray"
        ];

        "custom/spacer" = {
          format = "   ";
        };

        "wlr/taskbar" = {
          all-outputs = false;
          format = "{icon}";
          icon-size = 13;
          tooltip = true;
          tooltip-format = "{title}";
          active-first = false;
        };

        cpu = {
          format = "{usage}%  ";
          tooltip = false;
        };

        memory = {
          interval = 30;
          format = "󰾆  {used}GB";
          format-m = "󰾅  {used}GB";
          format-h = "󰓅  {used}GB";
          format-c = "  {used}GB";
          max-length = 10;
          tooltip = true;
          tooltip-format = "󰾆 {percentage}%\n {used:0.1f}GB/{total:0.1f}GB";
        };

        backlight = {
          format = "{icon} {percent}%";
          format-icons =  ["" "" "" "" "" "" "" "" ""];
          on-scroll-up = "${lib.getExe pkgs.brightnessctl} s +1%";
          on-scroll-down = "${lib.getExe pkgs.brightnessctl} s 1%-";
          min-length = 6;
        };

        battery = {
          states = {
            good = 95;
            warning = 30;
            critical = 20;
          };
          format = "{icon} {capacity}%";
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
          format-alt = "{time} {icon}";
          format-icons = [ "" "" "" "" "" ];
          format-time = "{H}h {M}min";
        };

        # TODO: bluetooth

        clock = {
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            on-scroll = 1;
            actions = {
              on-click-right = "mode";
              on-scroll-down = "shift_down";
              on-scroll-up = "shift_up";
            };
            format = {
              days = "<span color='{{color4}}'><b>{}</b></span>";
              months = "<span color='{{foreground}}'><b>{}</b></span>";
              today = "<span color='{{color3}}'><b><u>{}</u></b></span>";
              weekdays = "<span color='{{color5}}'><b>{}</b></span>";
            };
          };
          format = "<b>  {:%a %d %b     %H:%M}</b>";
          format-alt = "{:%H:%M %Y-%m-%d}";
          interval = 10;
          tooltip-format = "<tt><small>{calendar}</small></tt>";
        };

        tray = {
          icon-size = 18;
          spacing = 10;
        };

        #"custom/nix" = {
        #  format = "󱄅";
        #  on-click = "hypr-wallpaper";
        #  on-click-right = "wallpapers-select";
        #  tooltip = false;
        #};

        #idle_inhibitor = lib.mkIf cfg.idleInhibitor {
        #  format = "{icon}";
        #  format-icons = {
        #    activated = alertSpan "";
        #    deactivated = "";
        #  };
        #};

         "hyprland/workspaces" = {
            disable-scroll = false;
            all-outputs = false;
            active-only = false;
            format = "<span><b>{icon}</b></span>";
            format-icons = {
              "1" = "1";
              "2" = "2";
              "3" = "3";
              "4" = "4";
              "5" = "5";
              "6" = "6";
              "7" = "7";
              "8" = "8";
              "9" = "9";
              urgent = " ";
            };
          };

        "hyprland/window" = {
          max-length = 50;
          format = "<i>{title}</i>";
          separate-outputs = true;
          icon = true;
          icon-size = 13;
        };

        network = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = "{ipaddr}/{cidr} 󰈀 ";
          format-linked = "{ifname} (No IP)";
          format-disconnected = alertSpan "Disconnected ⚠";
          format-alt = "{ifname}: {ipaddr}";
          tooltip = true;
          tooltip-format = ''
            IP: <b>{ipaddr}/{cidr}</b>
            Gateway: <b>{gwaddr}</b>'';

          on-click = "${config.xdg.configHome}/rofi/rofi-wifi-menu"; # FIXME
          on-click-right = "nmtui"; #FIXME
        };

        # See more: https://github.com/prasanthrangan/hyprdots/blob/main/Configs/.config/waybar/modules/pulseaudio.jsonc
        pulseaudio = {
          format = "{volume}% {icon} {format_source}";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = " {icon} {format_source}";
          format-muted = " {format_source}";
          format-source = "{volume}% ";
          format-source-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
          on-click = "${lib.getExe pkgs.pavucontrol}";

          tooltip = true;
          tooltip-format = "{icon} {desc} // {volume}%";
        };

        #"custom/powermenu" = {
        #  "format" = "";
        #  "on-click" = "pkill rofi || ~/.config/rofi/powermenu/type-3/powermenu.sh";
        #  "tooltip" = false;
        #};


        # Notification: https://github.com/prasanthrangan/hyprdots/blob/main/Configs/.config/waybar/modules/notifications.jsonc
      };
    };
  };
}
