{ config, pkgs, lib, self, ... }:
let
  theme = config.custom.theme;
  palette = theme.palette;
  font = theme.fonts.monospace;
in
{
  programs.fuzzel = {
    enable = true;
    settings = {
      main.font = font.name;
      colors = {
        background = "1e1e2edd";
        text = "cdd6f4ff";
        match = "f38ba8ff";
        selection = "585b70ff";
        selection-match = "f38ba8ff";
        selection-text = "cdd6f4ff";
        border = "b4befeff";
      };
    };
  };
}
