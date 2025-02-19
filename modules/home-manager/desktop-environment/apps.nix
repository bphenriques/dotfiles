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

  volumeOpt = lib.types.submodule {
    options = {
      increase    = mkAppOpt { description = "Increase volume"; };
      decrease    = mkAppOpt { description = "Decrease volume"; };
      toggle-mute = mkAppOpt { description = "Mute volume"; };
    };
  };

  brightnessOpt = lib.types.submodule {
    options = {
      increase = mkAppOpt { description = "Increase brightness"; };
      decrease = mkAppOpt { description = "Decrease brightness"; };
    };
  };
  
  mediaPlayerOpt = lib.types.submodule {
    options = {
      previous    = mkAppOpt { description = "Go to previous track"; };
      next        = mkAppOpt { description = "Go to next next"; };
      play-pause  = mkAppOpt { description = "Play/pause current track"; };
    };
  };


  mediaOpt = lib.types.submodule {
    options = {
      previous      = mkAppOpt { description = "Previous track"; };
      next          = mkAppOpt { description = "Next track"; };
      toggle-pause  = mkAppOpt { description = "Toggle Pause"; };
    };
  };

  coreOpt = lib.types.submodule {
    options = {
      application-launcher  = mkAppOpt { description = "Application launcher"; };
      file-browser          = mkAppOpt { description = "File Browser"; };
      window-switcher       = mkAppOpt { description = "Window switcher"; };
      session-menu          = mkAppOpt { description = "Session Menu"; };
      terminal              = mkAppOpt { description = "Terminal"; };
      screen-lock           = mkAppOpt { description = "Screen Lock"; };
      system-monitor        = mkAppOpt { description = "System Monitor"; };
    };
  };

  toolsOpt = lib.types.submodule {
    options = {
      system-monitor        = mkAppOpt { description = "System Monitor"; };
      emoji-picker          = mkAppOpt { description = "Dmenu runner"; };
      screenshot-menu       = mkAppOpt { description = "Screenshot menu"; };
    };
  };
in
{
  options.custom.desktop-environment.apps = {
    volume = lib.mkOption {
      description = "Manage volume";
      type = volumeOpt;
      default = {
        increase    = "${volume} increase";
        decrease    = "${volume} decrease";
        toggle-mute = "${volume} toggle-mute";
      };
    };

    brightness = lib.mkOption {
      description = "Manage brightness";
      type = brightnessOpt;
      default = {
        increase    = "${brightness} increase";
        decrease    = "${brightness} decrease";
      };
    };

    mediaPlayer = lib.mkOption {
      description = "Manage brightness";
      type = mediaPlayerOpt;
      default = {
        previous    = "${playerctl} previous";
        next        = "${playerctl} next";
        play-pause  = "${playerctl} play-pause";
      };
    };
    
    # Core
    application-launcher  = mkAppOpt { description = "Application launcher"; };
    file-browser          = mkAppOpt { description = "File Browser"; };
    window-switcher       = mkAppOpt { description = "Window switcher"; };
    session-menu          = mkAppOpt { description = "Session Menu"; };
    terminal              = mkAppOpt { description = "Terminal launcher"; };
    screen-lock           = mkAppOpt { description = "Screen Lock"; };
    system-monitor        = mkAppOpt { description = "System Monitor"; };

    # Tools
    emoji-picker          = mkAppOpt { description = "Dmenu runner"; };
    screenshot-menu       = mkAppOpt { description = "Screenshot menu"; };
  };
}