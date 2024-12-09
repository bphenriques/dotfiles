{ config, lib, pkgs, ... }:
{
  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    settings = {
      global = {
        enable_recursive_icon_lookup = true;
        rounded = true;
        width = "400";
        height = "400";
        gap_size = 2;
        progress_bar_corner_radius = 2;
        idle_threshold = 120;
        markup = "full";
        corner_radius = 10;
        follow = "mouse";

        font = "Source Sans Pro 10";
        frame_color = "#232323";
        frame_width = 1;
        offset = "15x15";
        mouse_left_click = "do_action, close_current";
        mouse_middle_click = "close_current";
        mouse_right_click = "close_all";
      };

      fullscreen_delay_everything = {
        fullscreen = "delay";
      };

      urgency_critical = {
        background = "#d64e4e";
        foreground = "#f0e0e0";
      };
      urgency_low = {
        background = "#232323";
        foreground = "#2596be";
      };
      urgency_normal = {
        background = "#1e1e2a";
        foreground = "#2596be";
      };
    };
  };
}