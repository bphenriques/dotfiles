{ lib, pkgs, config, ... }:
let
  cfg = config.custom.programs.satty;
  tomlFormat = pkgs.formats.toml { };
in
{
  options.custom.programs.satty = {
    enable = lib.mkEnableOption "satty";
    directory = lib.mkOption {
      description = "Location of screenshots";
      type = lib.types.str;
    };

    format = lib.mkOption {
      description = "Filename format of screenshots. Templates must be compatible with strftime";
      type = lib.types.str;
      default = "screenshot-%Y%m%d-%H%M%S.png";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.satty" pkgs lib.platforms.linux) ];

    home.packages = [ pkgs.satty ];

    xdg.configFile."satty/config.toml".source = tomlFormat.generate "satty-config" {
      general = {
        fullscreen = true;
        early-exit = true;
        disable-notifications = false;
        initial-tool = "brush";
        copy-command = "wl-copy";
        save-after-copy = false;
        output-filename = "${cfg.directory}/${cfg.format}";
      };
    };
  };
}
