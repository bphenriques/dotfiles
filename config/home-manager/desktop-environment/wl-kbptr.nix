{ config, lib, pkgs, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;

  defaultOpacity = "66"; # 40%
  floatingOpacity = "D9"; # 85%
  unselectableBackgroundColor = colors.base04;
  labelColor = colors.base06;
  floatingLabelColor = colors.base01;
  evenSelectedLabelColor = colors.base0A;
  evenBackgroundColor = colors.base0B;
  evenBorderColor = colors.base01;
  oddBackgroundColor = colors.base0D;
  oddBorderColor = colors.base01;
  historyBorderColor = colors.base03;
in {
  home.packages = [ pkgs.wl-kbptr ];

  xdg.configFile."wl-kbptr/config".text = lib.generators.toINI { } {
    general = {
      home_row_keys = "";
      modes = "tile,bisect";
    };

    mode_tile = {
      label_font_family = config.stylix.fonts.sansSerif.name;
      label_font_size = "8 50% 100";
      label_color = labelColor;
      label_select_color = evenSelectedLabelColor;
      unselectable_bg_color = unselectableBackgroundColor + "${defaultOpacity}";
      selectable_bg_color = evenBackgroundColor + "${defaultOpacity}";
      selectable_border_color = evenBorderColor;
    };

    mode_floating = {
      source = "detect";
      label_color = floatingLabelColor;
      label_select_color = evenSelectedLabelColor;
      unselectable_bg_color = unselectableBackgroundColor + "${floatingOpacity}";
      selectable_bg_color = evenBackgroundColor + "${floatingOpacity}";
      selectable_border_color = evenBorderColor;
      label_font_family = config.stylix.fonts.sansSerif.name;
      label_font_size = "15 50% 100";
    };

    mode_bisect = {
      label_font_family = config.stylix.fonts.sansSerif.name;
      label_color = colors.base07;
      label_font_size = 20;
      label_padding = 12;
      pointer_size = 20;
      pointer_color = colors.base08;
      unselectable_bg_color = unselectableBackgroundColor + "${defaultOpacity}";
      even_area_bg_color = evenBackgroundColor + "${defaultOpacity}";
      even_area_border_color = evenBorderColor;
      odd_area_bg_color = oddBackgroundColor + "${defaultOpacity}";
      odd_area_border_color = oddBorderColor;
      history_border_color = historyBorderColor;
    };

    # FIXME
    mode_split = {
      pointer_size = 20;
      pointer_color = colors.base08;
      bg_color = "#2226";
      area_bg_color = "11111188";
      vertical_color = evenBackgroundColor;
      horizontal_color = oddBackgroundColor;
      history_border_color = historyBorderColor;
    };
  };

  # TODO: Move to niri submaps + wlrctl once we have https://github.com/YaLTeR/niri/issues/846
  custom.programs.niri.bindings = {
    "Mod+Alt+M" = ''spawn "${lib.getExe pkgs.wl-kbptr}"'';
  };
}
