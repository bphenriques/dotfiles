{ config, lib, pkgs, ... }:

let
  foreground            = "#bbc2cf";
  background            = "#282c34";
  selection_foreground  = "#bbc2cf";
  selection_background  = "#3f444a";

  cursor                = "#bbc2cf";
  cursor_text_color     = "#282c34";

  # Black
  color0 = "#282c34";
  color8 = "#3f444a";

  # Red
  color1 = "#ff6c6b";
  color9 = "#ff6655";

  # Green
  color2  = "#98be65";
  color10 = "#99bb66";

  # Yellow
  color3  = "#ECBE7B";
  color11 = "#ECBE7B";

  # Blue
  color4  = "#51afef";
  color12 = "#51afef";

  # Magenta
  color5  = "#c678dd";
  color13 = "#c678dd";

  # Cyan
  color6  = "#46D9FF";
  color14 = "#46D9FF";

  # White
  color7  = "#dfdfdf";
  color15 = "#bbc2cf";
in
{
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      return {
        term = 'screen-256color',
        font = wezterm.font('Hack Nerd Font Mono'),
        font_size = 13,
        color_scheme = 'doom-one',
        enable_tab_bar = false, -- use tmux
        window_close_confirmation = 'NeverPrompt',
        window_padding = {
          left = '1cell',
          right = '1cell',
          top = '0.5cell',
          bottom = '0', -- use tmux
        }
      }
    '';
    colorSchemes = {
      doom-one = {
        ansi    = [color0 color1 color2   color3  color4  color5  color5  color7];
        brights = [color8 color9 color10  color11 color12 color13 color14 color15];
        background = background;
        cursor_bg = cursor;
        # cursor_border = "#BEAF8A";
        cursor_fg = cursor_text_color;
        foreground = foreground;
        selection_bg = selection_background;
        selection_fg = selection_foreground;
      };
    };
  };
}
