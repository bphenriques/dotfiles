{ pkgs, lib, config, ... }:
let
  inherit (config.lib.stylix) colors;
  inherit (config) stylix;

  backgroundOpacity = "F2"; # 95%
in
{
  services.dunst = {
    enable = true;
    iconTheme = {
      name = if (stylix.polarity == "dark") then stylix.iconTheme.dark else stylix.iconTheme.light;
      package = stylix.iconTheme.package;
    };
    settings = {
      global = {
        # Style
        font = "${stylix.fonts.sansSerif.name} ${toString stylix.fonts.sizes.popups}";
        background = colors.withHashtag.base00 + backgroundOpacity;
        foreground = colors.withHashtag.base05;
        highlight = "${colors.withHashtag.base02}, ${colors.withHashtag.base02}";
        width = 350;
        height = "(0, 300)";
        origin = "top-right";
        offset = "(35, 35)";
        gap_size = 12;
        padding = 12;
        horizontal_padding = 20;
        frame_width = 2;
        icon_position = "left";
        min_icon_size = 64;
        max_icon_size = 64;
        progress_bar_frame_width = 0;
        progress_bar_corner_radius = 3;
        alignment = "left";
        vertical_alignment = "center";
        format = "<big><b>%s</b></big>\n%b";
        corner_radius = 10;

        # Behaviour
        follow = "mouse";
        indicate_hidden = "yes";
        notification_limit = 5;
        sort = "no";
        show_age_threshold = -1;
        hide_duplicate_count = false;
        markup = "full";
        dmenu = ''${lib.getExe pkgs.fuzzel} --dmenu -p "Open with"'';
        browser = lib.getExe' pkgs.xdg-utils "xdg-open";
        mouse_left_click = "do_action, close_current";
        mouse_middle_click = "do_action, close_current";
        mouse_right_click = "close_all";
      };

      urgency_low = {
        frame_color = colors.withHashtag.base0B + backgroundOpacity;
        timeout = 3;
      };

      urgency_normal = {
        frame_color = colors.withHashtag.base0D + backgroundOpacity;
        timeout = 8;
      };

      urgency_critical = {
        frame_color = colors.withHashtag.base08 + backgroundOpacity;
        highlight = "${colors.withHashtag.base0F}, ${colors.withHashtag.base08}";
        timeout = 0;
      };

      fullscreen_delay_everything.fullscreen = "delay";
      fullscreen_show_critical = {
        msg_urgency = "critical";
        fullscreen = "show";
      };
    };
  };

  custom.programs.niri.layerRules.screencasting.block = [ ''namespace="^notifications$"'' ];
}

