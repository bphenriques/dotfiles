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
  weatherFrequencySeconds = 30 * 60;
  weatherCommand = "curl 'wttr.in/${weatherLocation}?format=%c%t%20${weatherLocation}' | sed 's/\+//g'";
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

      hide_frame_for_single_pane "false"

      mode_normal  ""
      mode_tab "#[bg=${green},fg=${black}] {name} "
      mode_default_to_mode "tmux"

      tab_normal   "#[fg=${yellow}] {index} #[fg=${brightWhite}] {name} "
      tab_active   "#[bg=${yellow},fg=${black}] {index} #[bg=${brightBlack},fg=${white}] {name} "
      tab_separator "  "

      command_weather_command "bash -c \" ${weatherCommand}\""
      command_weather_format "{stdout}"
      command_weather_interval "${toString weatherFrequencySeconds}"
      command_weather_rendermode  "static"

      datetime        "#[bg=${blue},fg=${black}]  {format} "
      datetime_format "%a %d %b %H:%M"
      datetime_timezone "Europe/Lisbon"
    }
  }
}
''
