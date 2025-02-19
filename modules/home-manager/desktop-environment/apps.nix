{ lib, pkgs, config, self, ... }:

let
  cfg = config.custom.desktop-environment.apps;

  volume = lib.getExe self.pkgs.volume-osd;
  brightness = lib.getExe self.pkgs.brightness-osd;
  playerctl = lib.getExe pkgs.playerctl;

  mkAppOpt = { description, default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };
in
{
  options.custom.desktop-environment.apps = {
    volume = {
      increase    = mkAppOpt { description = "Increase volume"; default = "${volume} increase"; };
      decrease    = mkAppOpt { description = "Decrease volume"; default = "${volume} decrease"; };
      toggle-mute = mkAppOpt { description = "Mute volume";     default = "${volume} toggle-mute"; };
    };

    brightness = {
      increase = mkAppOpt { description = "Increase brightness"; default = "${brightness} increase"; };
      decrease = mkAppOpt { description = "Decrease brightness"; default = "${brightness} decrease"; };
    };

    mediaPlayer = {
      previous      = mkAppOpt { description = "Previous track";  default = "${playerctl} previous"; };
      next          = mkAppOpt { description = "Next track";      default = "${playerctl} next"; };
      play-pause    = mkAppOpt { description = "Toggle Pause";    default = "${playerctl} toggle-pause"; };
    };

    core = {
      application-launcher  = mkAppOpt { description = "Application launcher"; };
      file-browser          = mkAppOpt { description = "File Browser"; };
      window-switcher       = mkAppOpt { description = "Window switcher"; };
      session-menu          = mkAppOpt { description = "Session Menu"; };
      terminal              = mkAppOpt { description = "Terminal"; };
      screen-lock           = mkAppOpt { description = "Screen Lock"; };
    };

    tools = {
      system-monitor        = mkAppOpt { description = "System Monitor"; };
      emoji-picker          = mkAppOpt { description = "Dmenu runner"; };
      screenshot-menu       = mkAppOpt { description = "Screenshot menu"; };
    };
  };
}