{ lib, config, pkgs, self, ... }:
let
  modules = {
    cpu = {
      format = "{usage}%  ";
    };

    temperature = {
      thermal-zone = 2;
      critical-threshold = 80;
      format = "{icon} {temperatureC}°C";
      format-icons = ["" "" "" "" ""];
    };

    memory = {
      interval = 30;
      format = "  {usage}%";
      max-length = 10;
      tooltip = true;
      tooltip-format = " {used:0.1f}GB/{total:0.1f}GB";
    };

    # FIXME: mkIf laptop
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

    power-profiles-daemon = {
      format = "{icon}";
      tooltip-format = "{profile}";
      tooltip = true;
      format-icons = {
        default = "";
        performance = "";
        balanced = "";
        power-saver = "";
      };
      min-length = 6;
    };

    "custom/os" = {
      format = "";
      on-click = "${lib.getExe pkgs.fuzzel}";
    };

    "niri/language" = {
      format-en = "  US";
      format-pt = "  PT";
      on-click-release = "${lib.getExe pkgs.niri} msg action switch-layout next";
    };

    "custom/media" = {
      format = "{icon} {}";
      escape = true;
      return-type = "json";
      max-length = 40;
      on-click = "playerctl play-pause";
      on-click-right = "playerctl stop";
      smooth-scrolling-threshold = 10;
      on-scroll-up = "playerctl next";
      on-scroll-down = "playerctl previous";
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
      on-click = "${lib.getExe pkgs.foot} --title=nmtui-tui ${lib.getExe' pkgs.networkmanager "nmtui"}";
    };

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

    pulseaudio = {
      format = "{icon} {volume}%";
      format-bluetooth = "{icon}";
      format-bluetooth-muted = " {icon}";
      format-source = " {volume}%";
      format-source-muted = " ";
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

      on-scroll-up = "${lib.getExe self.pkgs.volume-osd} increase";
      on-scroll-down ="${lib.getExe self.pkgs.volume-osd} decrease";

      tooltip = true;
      tooltip-format = "{desc}";
    };
  };

  groups = {
    "group/stats" = {
      orientation = "inherit";
      modules = [ "disk" "memory" "cpu" "temperature" ];
    };
    "group/toggles" = {
      orientation = "inherit";
      modules = [ "niri/language" "power-profiles-daemon" "pulseaudio" "network" "battery" ];
    };
  };
in
{
  systemd.user.services.waybar.Unit.After = [ config.wayland.systemd.target ]; # Fix ordering.
  programs.waybar = {
    enable = true;
    systemd = {
      enable = true;
      target = config.wayland.systemd.target;
    };
    style = ./style.css;    # Not using pkgs.writeText as having the file is handy to debug: waybar -s style.css
    settings = {
      default = lib.mergeAttrsList [
        modules
        groups
        {
          reload_style_on_change = true;

          layer = "top";
          position = "top";
          margin-left = 5;
          margin-right = 5;
          margin-top = 5;
          margin-bottom = 0;
          spacing = 5;

          modules-left    = [ "custom/os" ];
          modules-center  = [ "custom/media" ];
          modules-right   = [ "group/toggles" "clock" ];
        }
      ];
    };
  };
}
