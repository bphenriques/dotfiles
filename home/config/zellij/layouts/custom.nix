{  pkgs, lib, ... }:
# More icons: https://www.nerdfonts.com/cheat-sheet
let
  black = "#282c34";
  brightBlack = "#3f444a";
  yellow = "#ECBE7B";

  white  = "#dfdfdf";
  brightWhite = "#bbc2cf";

  red = "#ec867b";
  blue = "#7ba9ec";
  green = "#7BECBE";

  weatherLocation = "Lisbon";
  weatherCommand = "curl 'wttr.in/${weatherLocation}?format=%c%t%20${weatherLocation}' | sed 's/\+//g'";

  # https://zellij.dev/documentation/keybindings-modes.html?highlight=tmux#modes
  nonNormalModes = ["locked" "resize" "pane" "move" "tab" "scroll" "search" "entersearch" "renametab" "renamepane" "session" "tmux"];
in
''
layout {
  pane split_direction="vertical" {
      pane
  }

  pane size=1 borderless=true {
    plugin location="file:${pkgs.zjstatus}/bin/zjstatus.wasm" {
      format_left  " {tabs}"
      format_right "{mode} {command_weather} {datetime}"
      format_space ""

      border_enabled  "false"
      border_char     "─"
      border_format   "#[fg=${yellow}]{char}"
      border_position "top"

      hide_frame_for_single_pane "true"

      mode_normal  ""
      ${lib.concatMapStringsSep "\n" (mode: ''mode_${mode} "#[bg=${green},fg=${black}] {name} "'') nonNormalModes}

      tab_normal   "#[fg=${yellow}] {index} #[fg=${brightWhite}] {name} #[] "
      tab_active   "#[bg=${yellow},fg=${black}] {index} #[bg=${brightBlack},fg=${white}] {name} #[] "

      command_weather_command "bash -c \" ${weatherCommand}\""
      command_weather_format "{stdout}"
      command_weather_interval "${toString (60 * 30)}"
      command_weather_rendermode  "static"

      datetime        "#[bg=${blue},fg=${black}]  {format} "
      datetime_format "%a %d %b %H:%M"
      datetime_timezone "Europe/Lisbon"
    }
  }
}
''


# Right, hostname?

#setw -g window-status-current-format "#[bg=yellow fg=black] #I #[bg=brightblack fg=white] #W #{?window_zoomed_flag,#[bg=#7ba9ec fg=black],#{?pane_synchronized,#[bg=#7ba9ec fg=black],}}#{?pane_synchronized,B,}#{?window_zoomed_flag,Z,}"

# black: #282c34
# Yellow:

# tab_normal   "#[fg=#6C7086] {name} "
# tab_active   "#[fg=#9399B2,bold,italic] {name} "

#setw -g window-status-format "#[fg=yellow] #I #[fg=white dim] #W #{?window_zoomed_flag,#[fg=#7ba9ec],#{?pane_synchronized,#[fg=#7ba9ec],}}#{?pane_synchronized,B,}#{?window_zoomed_flag,Z,}#{?window_activity_flag,##,}"
#setw -g window-status-current-format "#[bg=yellow fg=black] #I #[bg=brightblack fg=white] #W #{?window_zoomed_flag,#[bg=#7ba9ec fg=black],#{?pane_synchronized,#[bg=#7ba9ec fg=black],}}#{?pane_synchronized,B,}#{?window_zoomed_flag,Z,}"

