{ config, lib, pkgs, ... }:
let
  cmd = desc: cmd: { inherit desc cmd; keep_open = true; };
  colors = config.lib.stylix.colors.withHashtag;
  backgroundOpacity = "26"; # 15%
  textOpacity = "BF"; # 75%
in {
  home.packages = [
    pkgs.wl-kbptr   # Select with g, h, b -> left, right and middle
  ];

  xdg.configFile."wl-kbptr/config".text = lib.generators.toINI { } {
    mode_tile = {
      label_color = colors.base07 + "${textOpacity}";
      label_select_color = colors.base0A + "${textOpacity}";
      unselectable_bg_color = colors.base04 + "${backgroundOpacity}";
      selectable_bg_color = colors.base0D + "${backgroundOpacity}";
      selectable_border_color = colors.base0D + "${backgroundOpacity}";
    };

    mode_bisect = {
      label_color = colors.base07 + "${textOpacity}";
      label_font_size = 20;
      label_padding = 12;
      pointer_size = 20;
      pointer_color = colors.base08 + "${textOpacity}";
      unselectable_bg_color = colors.base04 + "${backgroundOpacity}";
      even_area_bg_color = colors.base0B + "${backgroundOpacity}";
      even_area_border_color = colors.base0B + "${backgroundOpacity}";
      odd_area_bg_color = colors.base0D + "${backgroundOpacity}";
      odd_area_border_color = colors.base0D + "${backgroundOpacity}";
      history_border_color = colors.base0B + "${backgroundOpacity}";
    };
  };

  # TODO: Move to niri submaps + wlrctl once we have https://github.com/YaLTeR/niri/issues/846
  custom.programs.niri.bindings = {
    "Mod+Alt+M" = ''spawn "${lib.getExe pkgs.wl-kbptr}"'';
  };
}