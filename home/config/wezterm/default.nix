{ ... }:

let
  term = "screen-256color";
  font = "Hack Nerd Font Mono";
  fontSize = 14;

  foreground            = "#bbc2cf";
  background            = "#282c34";
  selectionForeground   = "#bbc2cf";
  selectionBackground   = "#3f444a";

  cursor                = "#bbc2cf";
  cursorTextColor       = "#282c34";

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
        term = '${term}',
        font = wezterm.font('${font}'),
        font_size = ${toString fontSize},
        color_scheme = 'doom-one',
        window_decorations = 'RESIZE',
        enable_tab_bar = false,
        window_close_confirmation = 'NeverPrompt',
        window_padding = {
          left = '0',
          right = '0',
          top = '0',
          bottom = '0',
        },
        keys = {
          -- Make Option-Left equivalent to Alt-b which many line editors interpret as backward-word
          {key="LeftArrow", mods="OPT", action=wezterm.action{SendString="\x1bb"}},
          -- Make Option-Right equivalent to Alt-f; forward-word
          {key="RightArrow", mods="OPT", action=wezterm.action{SendString="\x1bf"}},
        }
      }
    '';
    colorSchemes = {
      doom-one = {
        ansi    = [color0 color1 color2   color3  color4  color5  color5  color7];
        brights = [color8 color9 color10  color11 color12 color13 color14 color15];
        background = background;
        cursor_bg = cursor;
        cursor_fg = cursorTextColor;
        foreground = foreground;
        selection_bg = selectionBackground;
        selection_fg = selectionForeground;
      };
    };
  };
}
