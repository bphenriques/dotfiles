{ lib, pkgs, config, self, osConfig, ... }:
let
  cfg = config.custom.programs.screenshot;

  screenshot = lib.getExe cfg.package;
  exec = {
    screen        = ''${screenshot} ${cfg.directory} screen'';
    region        = ''${screenshot} ${cfg.directory} region'';
    window        = ''${screenshot} ${cfg.directory} window'';
  };
in
{
  options.custom.programs.screenshot = {
    enable = lib.mkEnableOption "custom-screenshot";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.screenshot;
    };
    directory = lib.mkOption {
      description = "Location of screenshots";
      type = lib.types.str;
    };

    exec = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      default = { inherit (exec) screen region window; };
      description = "Screenshot commands for use in keybindings";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.screenshot" pkgs lib.platforms.linux) ];

    home.packages = [ cfg.package ];

    # Suppress niri's built-in screenshot notification (we show our own with an edit action).
    services.dunst.settings.niri_screenshot = {
      appname = "niri";
      summary = "Screenshot captured";
      skip_display = true;
      history_ignore = true;
    };

  };
}
