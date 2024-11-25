{ lib, config, pkgs, ... }:
# https://codeberg.org/explosion-mental/wallust generates colors autoamtically.
# https://github.com/bitSheriff/dotfiles/blob/master/configuration/.config/waybar/modules/modules.jsonc
# Notification: https://github.com/prasanthrangan/hyprdots/blob/main/Configs/.config/waybar/modules/notifications.jsonc


# pkill waybar && hyprctl dispatch exec waybar
{
  # https://github.com/nix-community/home-manager/issues/3599
  # do not use the systemd service as it is flaky and unreliable

  programs.waybar = {
    enable = true;
    style = ./style.css;
    settings = {
      "bar" = {
        #output = ["eDP-1" "HDMI-A-1"];
        reload_style_on_change = true;
        layer = "top";
        position = "top";
        margin-left = 0;
        margin-right = 0;
        margin-top = 0;
        margin-bottom = 0;
        spacing = 2;


        modules-left = [
          "custom/os"
          "niri/workspaces"
        ];
        modules-center = [
        ];
        modules-right = [
          "tray"
          "pulseaudio"
          "battery"
          #"hyprland/language" # FIXME: for niri
          "network"

          # List of unsure widgets
          "memory"
          "cpu"
          "disk"
          "backlight"
          "power-profiles-daemon"

          "clock"
        ];

        "hyprland/language" = {
          format-en = "US";
          format-pt = "PT";
        };

        "custom/os" = {
          format = "";
          on-click = "fuzzel";
          tooltip = false;
          #menu = "on-click";
          #menu-file = ./menu/power_menu.xml;
          #menu-actions = {
          #  shutdown = "shutdown";
          #  reboot = "reboot";
          #  suspend = "systemctl suspend";
          #  hibernate = "systemctl hibernate";
          #};
        };

        "custom/media" = {
          format = "{icon} {}";
          escape = true;
          return-type = "json";
          max-length = 40;
          on-click = "playerctl play-pause";
          on-click-right = "playerctl stop";
          smooth-scrolling-threshold = 10; # // This value was tested using a trackpad, it should be lowered if using a mouse.
          on-scroll-up = "playerctl next";
          on-scroll-down = "playerctl previous";
          exec = "$HOME/.config/waybar/mediaplayer.py 2> /dev/null"; # // Script in resources/custom_modules folder
        };

        #"wlr/taskbar" = {
        #  all-outputs = false;
        #  format = "{icon}";
        #  icon-size = 13;
        #  tooltip = true;
        #  tooltip-format = "{title}";
        #  active-first = false;
        #};

        cpu = {
          format = "{usage}%  ";
          tooltip = false;
        };

        temperature = {
          thermal-zone = 2;
          critical-threshold = 80;
          format = "{temperatureC}°C ";
        };

       disk = {
         interval = 30;
         format = "󰋊 {percentage_used}%";
         path = "/";
         tooltip = true;
         unit = "GB";
         tooltip-format = "Available {free} of {total}";
       };

        memory = {
          interval = 30;
          format = "  {usage}%";
          max-length = 10;
          tooltip = true;
          tooltip-format = " {used:0.1f}GB/{total:0.1f}GB";
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
          on-click = "${lib.getExe pkgs.wlogout} &";
        };

        # TODO: bluetooth

        clock = {
          format = "{:%a %d %b  %H:%M}";
          interval = 10;
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
             mode = "year";
             mode-mon-col = 3;
             weeks-pos = "right" ;
             on-scroll = 1;
             on-click-right = "mode";
             format = {
               months = "<span color='#ffead3'><b>{}</b></span>";
               days = "<span color='#ecc6d9'><b>{}</b></span>";
               weeks = "<span color='#99ffdd'><b>W{}</b></span>";
               weekdays = "<span color='#ffcc66'><b>{}</b></span>";
               today = "<span color='#ff6699'><b><u>{}</u></b></span>";
             };
          };
          actions = {
            on-click-right = "mode";
            on-click-forward = "tz_up";
            on-click-backward = "tz_down";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };

        tray = {
          icon-size = 18;
          spacing = 10;
        };

        "niri/workspaces" = {
          icon-size = 32;
          spacing = 16;
          all-outputs = false;
          on-scroll-up = "hyprctl dispatch workspace e+1";
          on-scroll-down = "hyprctl dispatch workspace e-1";
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

        power-profiles-daemon = {
          format = "{icon}";
          tooltip-format = "Power profile: {profile}\nDriver: {driver}";
          tooltip = true;
          format-icons = {
            default = "";
            performance = "";
            balanced = "";
            power-saver = "";
          };
          min-length = 6;
        };

        network = {
          format-wifi = " {icon}";
          format-ethernet = "  ";
          format-disconnected = "󰌙";
          format-icons = [ "󰤯 " "󰤟 " "󰤢 " "󰤢 " "󰤨 " ];
          tooltip = true;
          tooltip-format = ''
            <b>IP</b>: {ipaddr}/{cidr}
            <b>Gateway</b>: {gwaddr}'';

          on-click = "${config.xdg.configHome}/rofi/rofi-wifi-menu"; # FIXME
          on-click-right = "nmtui"; #FIXME
        };


        # See more: https://github.com/prasanthrangan/hyprdots/blob/main/Configs/.config/waybar/modules/pulseaudio.jsonc
        pulseaudio = {
          format = "{icon}";
          format-bluetooth = "{icon}";
          format-bluetooth-muted = " {icon}";
          #format-muted = " {format_source}";
          format-source = "";
          format-source-muted = "";
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
          tooltip-format = ''{icon} {volume}% - {desc}'';
        };
      };
    };
  };
}
