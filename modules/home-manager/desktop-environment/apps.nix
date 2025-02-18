{ lib, pkgs, config, self, ... }:

let
  cfg = config.custom.desktop-environment.apps;

  volume = lib.getExe self.pkgs.volume-osd;
  brightness = lib.getExe self.pkgs.brightness-osd;

  mkRunOption = description: lib.mkOption {
    inherit description;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  volumeOpt = lib.types.submodule {
    options = {
      increase    = mkRunOption "Increase volume";
      decrease    = mkRunOption "Decrease volume";
      toggle-mute = mkRunOption "Mute volume";
    };
  };

  brightnessOpt = lib.types.submodule {
    options = {
      increase = mkRunOption "Increase brightness";
      decrease = mkRunOption "Decrease brightness";
    };
  };

  mediaOpt = lib.types.submodule {
    options = {
      previous      = mkRunOption "Previous track";
      next          = mkRunOption "Next track";
      toggle-pause  = mkRunOption "Toggle Pause";
    };
  };

  coreOpt = lib.types.submodule {
    options = {
      application-launcher  = mkRunOption "Application launcher";
      file-browser          = mkRunOption "File Browser";
      window-switcher       = mkRunOption "Window switcher";
      session-menu          = mkRunOption "Session Menu";
      terminal              = mkRunOption "Terminal";
      screen-lock           = mkRunOption "Screen Lock";
      system-monitor        = mkRunOption "System Monitor";
    };
  };

  toolsOpt = lib.types.submodule {
    options = {
      system-monitor        = mkRunOption "System Monitor";
      emoji-picker          = mkRunOption "Dmenu runner";
      screenshot-menu       = mkRunOption "Screenshot menu";
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

    # Core
    application-launcher  = mkRunOption "Application launcher";
    file-browser          = mkRunOption "File Browser";
    window-switcher       = mkRunOption "Window switcher";
    session-menu          = mkRunOption "Session Menu";
    terminal              = mkRunOption "Terminal launcher";
    screen-lock           = mkRunOption "Screen Lock";
    system-monitor        = mkRunOption "System Monitor";

    # Tools
    emoji-picker          = mkRunOption "Dmenu runner";
    screenshot-menu       = mkRunOption "Screenshot menu";
  };
}