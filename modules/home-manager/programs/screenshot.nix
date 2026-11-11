{ lib, pkgs, config, self, ... }:
let
  cfg = config.my.programs.screenshot;

  screenshot = mode: "${lib.getExe cfg.package} ${lib.escapeShellArg "${cfg.directory}/${cfg.format}"} ${mode}";
in
{
  options.my.programs.screenshot = {
    enable = lib.mkEnableOption "screenshots with an edit action";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.screenshot;
    };
    directory = lib.mkOption {
      description = "Location of screenshots";
      type = lib.types.str;
    };

    format = lib.mkOption {
      description = "Screenshot filename format (strftime)";
      type = lib.types.str;
      default = "screenshot-%Y%m%d-%H%M%S.png";
    };

    exec = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      default = lib.genAttrs [ "screen" "region" "window" ] screenshot;
      description = "Screenshot commands for use in keybindings";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "my.programs.screenshot" pkgs lib.platforms.linux) ];

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
