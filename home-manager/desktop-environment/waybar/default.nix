{ lib, config, pkgs, self, ... }:
# https://github.com/bitSheriff/dotfiles/blob/master/configuration/.config/waybar/modules/modules.jsonc
# https://github.com/nix-community/nur-combined/blob/4d8b064e3cff836ee8c17c48c592874b0209e167/repos/slaier/modules/waybar/mediaplayer.nix
# TODO: Notification: https://github.com/prasanthrangan/hyprdots/blob/main/Configs/.config/waybar/modules/notifications.jsonc
let
  audio = {
    headset = {
      name = "alsa_output.usb-SteelSeries_SteelSeries_Arctis_7-00.stereo-game";
      icon = "";
      icon-muted = "󰋐";
    };

    external-speaker = {
      name = "alsa_output.pci-0000_01_00.1.hdmi-stereo";
      icon = "󰓃";
      icon-muted = "󰓄";
    };

    internal-speaker = {
      name = "alsa_output.pci-0000_06_00.6.analog-stereo";
      icon = "󰽟";
      icon-muted = "󰽠";
    };
  };

  display = {
    external-only = "";
    internal-aptop = "";
    extended = "󱂬";
  };

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

    # FIXME: Does not work with ZFS
    disk = {
      interval = 30;
      format = " {percentage_used}%";
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
      # on-click = "${lib.getExe pkgs.wlogout} &"; FIXME
    };

    # FIXME: mkIf laptop
    backlight = {
      format = "{icon} {percent}%";
      format-icons =  ["" "" "" "" "" "" "" "" ""];
      on-scroll-up = "${lib.getExe self.pkgs.osd-brightness} increase";
      on-scroll-down ="${lib.getExe self.pkgs.osd-brightness} decrease";
      min-length = 6;
      # "on-click": "wdisplays"
    };

    # FIXME: mkIf laptop
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

      on-click = "${config.xdg.configHome}/rofi/rofi-wifi-menu"; # FIXME
      on-click-right = "nmtui"; #FIXME
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

    # See more: https://github.com/prasanthrangan/hyprdots/blob/main/Configs/.config/waybar/modules/pulseaudio.jsonc
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

        "${audio.headset.name}" = audio.headset.icon;
        "${audio.headset.name}-muted" = audio.headset.icon-muted;
        "${audio.external-speaker.name}" = audio.external-speaker.icon;
        "${audio.external-speaker.name}-muted" = audio.external-speaker.icon-muted;
        "${audio.internal-speaker.name}" = audio.internal-speaker.icon;
        "${audio.internal-speaker.name}-muted" = audio.internal-speaker.icon-muted;
      };
      on-click = "${lib.getExe pkgs.pavucontrol}";
      #on-right-click = "${lib.getExe self.pkgs.dunst-volume} set alsa_output.usb-SteelSeries_SteelSeries_Arctis_7-00.stereo-game";

      on-scroll-up = "${lib.getExe self.pkgs.osd-volume} increase";
      on-scroll-down ="${lib.getExe self.pkgs.osd-volume} decrease";

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
    #systemd.enable = false; # Run manually as it seems flaky: https://github.com/nix-community/home-manager/issues/3599
    style = ./style.css;    # Not using pkgs.writeText as having the file is handy to debug: waybar -s style.css
    settings = {
      default = lib.attrsets.mergeAttrsList [
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
