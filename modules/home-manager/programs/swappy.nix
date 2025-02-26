{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.programs.swappy;
in
{
  options.custom.programs.swappy = {
    enable = lib.mkEnableOption "swappy";
    directory = lib.mkOption {
      description = "Location of screenshots";
      type = lib.types.str;
      default = config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR;
    };

    format = lib.mkOption {
      description = "Filename format of screenshots. Templates must be compatible with the date command";
      type = lib.types.str;
      default = "screenshot-%Y%m%d-%H%M%S.png";
    };
  };

  config = {
   assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.swappy" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.swappy
      (pkgs.makeDesktopItem {
        type = "Application";
        name = "swappy-edit";
        exec = "${lib.getExe pkgs.swappy} -f %f";
        desktopName = "Annotate image";
        mimeTypes = config.custom.xdgDefaultApps.mimes.image;
      })
    ];

    xdg.configFile."swappy/config".text = lib.generators.toINI { } {
      Default = {
        save_dir = cfg.directory;
        save_filename_format = cfg.format;
        show_panel = true;
        early_exit = true;
        auto_save = true;
      };
    };
  };
}
