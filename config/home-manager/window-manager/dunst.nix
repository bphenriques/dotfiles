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

        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;
        line_height = 0;
        markup = "full";
        show_age_threshold = 60;
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        sticky_history = "yes";
        history_length = 20;
        always_run_script = true;
        corner_radius = 10;
        follow = "mouse";
        font = "Source Sans Pro 10";
        format = "<b>%s</b>\\n%b"; #format = "<span foreground='#f3f4f5'><b>%s %p</b></span>\n%b"
        frame_color = "#232323";
        frame_width = 1;
        offset = "15x15";
        horizontal_padding = 10;
        icon_position = "left";
        indicate_hidden = "yes";
        min_icon_size = 22;
        max_icon_size = 64;
        mouse_left_click = "do_action, close_current";
        mouse_middle_click = "close_current";
        mouse_right_click = "close_all";
        padding = 10;
        plain_text = "no";
        separator_height = 2;
        show_indicators = "yes";
        shrink = "no";
        word_wrap = "yes";
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