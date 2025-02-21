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
  };
}