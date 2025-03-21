{ pkgs, lib, config, ... }:
let
  inherit (config.lib.stylix) colors;
  inherit (config) stylix;

  backgroundOpacity = "F2"; # 95%

  mkMakoOsd = category: ''
    [category=${category}]
    anchor=top-center
    margin=10,0,10,0
  '';
in
{
  stylix.targets.mako.enable = config.stylix.enable;
  services.mako = {
    enable = false;
    layer = "overlay";
    defaultTimeout = 5000;

    # Theming is covered separately.
    width = 300;
    height = 200;
    margin = "8";
    padding = "12";
    borderSize = 1;
    borderRadius = 4;

    extraConfig = ''
      [urgency=critical]
      default-timeout=0

      ${mkMakoOsd "brightness-osd"}
      ${mkMakoOsd "volume-osd"}
    '';
  };

  #stylix.targets.dunst.enable = config.stylix.enable;

  # Old commit: https://github.com/bphenriques/dotfiles/pull/58/commits/7c0dd0766b206232ea027a32c3a952070e9e0b02#diff-2392b038587b0f643687f1c90b311af7278875629afc8198eb1f2fc6cef3c3e9R15
  services.dunst = {
    enable = true;
    iconTheme = {
      name = if (stylix.polarity == "dark") then stylix.iconTheme.dark else stylix.iconTheme.light;
      package = stylix.iconTheme.package;
    };
    settings = {
      global = {
        font = "${stylix.fonts.sansSerif.name} ${toString stylix.fonts.sizes.popups}";

        follow = "mouse";
        width = 350;
        height = "(0, 300)";

        origin = "top-right";
        offset = "(35, 35)";

        indicate_hidden = "yes";
        notification_limit = 5;
        gap_size = 12;
        padding = 12;
        horizontal_padding = 20;
        frame_width = 2;
        sort = "no";

        progress_bar_frame_width = 0;
        progress_bar_corner_radius = 3;

        # Coloring: not using stylix directly intentionally
        # background = colors.withHashtag.base01 + backgroundOpacity; #"#383c4af0";
        #foreground = colors.withHashtag.base05; #"#cdd1dc";

        # Not set by stylix
        highlight = "${colors.withHashtag.base03}, ${colors.withHashtag.base03}";

        icon_position = "left";
        min_icon_size = 64;
        max_icon_size = 64;

        markup = "full";
        format = "<big><b>%s</b></big>\n%b";
        alignment = "left";
        vertical_alignment = "center";
        show_age_threshold = -1;
        hide_duplicate_count = false;

        dmenu = ''${lib.getExe pkgs.fuzzel} --dmenu -p "Open with"'';
        browser = lib.getExe' pkgs.xdg-utils "xdg-open";

        corner_radius = 10;

        # Mouse
        mouse_left_click = "close_current";
        mouse_middle_click = "do_action, close_current";
        mouse_right_click = "close_all";
      };

      urgency_low = {
        background = colors.withHashtag.base01 + backgroundOpacity;
        foreground = colors.withHashtag.base05;
        frame_color = colors.withHashtag.base0B + backgroundOpacity;
        timeout = 3;
      };

      urgency_normal = {
        background = colors.withHashtag.base01 + backgroundOpacity;
        foreground = colors.withHashtag.base05;
        frame_color = colors.withHashtag.base0D + backgroundOpacity;
        timeout = 8;
      };

      urgency_critical = {
        background = colors.withHashtag.base01 + backgroundOpacity;
        foreground = colors.withHashtag.base05;
        frame_color = colors.withHashtag.base08 + backgroundOpacity;
        highlight = "${colors.withHashtag.base0F}, ${colors.withHashtag.base08}";
        timeout = 0;
      };

      fullscreen_delay_everything.fullscreen = "delay";
      fullscreen_show_critical = {
        msg_urgency = "critical";
        fullscreen = "show";
      };

      # TODO: Set max_icon_size just for Niri screenshots and similar
    };
  };

  custom.programs.niri.layerRules.screencasting.block = [ ''namespace="^notifications$"'' ];
}

