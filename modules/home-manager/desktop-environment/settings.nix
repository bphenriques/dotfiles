{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.desktop-environment.settings;

  fileBookmarkOpt = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      path = lib.mkOption { type = lib.types.str; };
    };
  };

  displayOutputOpt = lib.types.submodule {
    options = {
      identifier  = lib.mkOption { type = lib.types.str; };
      resolution  = lib.mkOption { type = lib.types.str; };
      refreshRate = lib.mkOption { type = lib.types.str; };
      scale       = lib.mkOption { type = lib.types.str; };
    };
  };
in
{
  options.custom.desktop-environment.settings = {
    file-bookmarks = lib.mkOption {
      description = "File bookmarks";
      type = lib.types.listOf fileBookmarkOpt;
    };

    displayOutput = lib.mkOption {
      description = "Default display output";
      type = displayOutputOpt;
    };

    screenshots = {
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

    screen-recorder = {
      directory = lib.mkOption {
        description = "Location of recordings";
        type = lib.types.str;
        default = config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR;
      };

      format = lib.mkOption {
         description = "Filename format of recordings. Templates must be compatible with the date command";
         type = lib.types.str;
         default = "record-%Y%m%d-%H%M%S.mp4";
       };
    };
  };
}