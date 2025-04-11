{ config, lib, pkgs, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;
  backgroundTransparency = "1A"; # 10%
in {
  home.packages = [
    pkgs.wl-kbptr
  ];

  xdg.configFile."wl-kbptr/config".text = lib.generators.toINI { } {
    general = {
      home_row_keys = "";
      modes = "floating,click";
    };

    mode_tile = {
      label_color = colors.base07;
      label_select_color = colors.base0A;
      unselectable_bg_color = colors.base04 + "${backgroundTransparency}";
      selectable_bg_color = colors.base0D + "${backgroundTransparency}";
      selectable_border_color = colors.base0D + "${backgroundTransparency}";
    };

    mode_floating = {
      source = "stdin";
      label_color = colors.base07;
      label_select_color = colors.base0A;
      unselectable_bg_color = colors.base04 + "${backgroundTransparency}";
      selectable_bg_color = colors.base0D + "${backgroundTransparency}";
      selectable_border_color = colors.base0D + "${backgroundTransparency}";
    #label_font_family=sans-serif
     # label_symbols=abcdefghijklmnopqrstuvwxyz      
    };

    mode_bisect = {
      label_color = colors.base07;
      label_font_size = 20;
      label_padding = 12;
      pointer_size = 20;
      pointer_color = colors.base08;
      unselectable_bg_color = colors.base04 + "${backgroundTransparency}";
      even_area_bg_color = colors.base0B + "${backgroundTransparency}";
      even_area_border_color = colors.base0B + "${backgroundTransparency}";
      odd_area_bg_color = colors.base0D + "${backgroundTransparency}";
      odd_area_border_color = colors.base0D + "${backgroundTransparency}";
      history_border_color = colors.base0B + "${backgroundTransparency}";
    };
  };

  # TODO: Move to niri submaps + wlrctl once we have https://github.com/YaLTeR/niri/issues/846
  # Select with g, h, b -> left, right and middle
  custom.programs.niri.bindings = {
    "Mod+Alt+M" = ''spawn "${lib.getExe pkgs.wl-kbptr}"'';
  };
}
