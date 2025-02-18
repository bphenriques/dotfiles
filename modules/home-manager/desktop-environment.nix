{ lib, pkgs, config, self, ... }:

let
  cfg = config.custom.desktop-environment;

  mkRunOption = description: lib.mkOption {
    inherit description;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  fileBookmark = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      path = lib.mkOption { type = lib.types.str; };
    };
  };
in
{
  options.custom.desktop-environment = {
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

    # Misc
    file-bookmarks = lib.mkOption {
      description = "File bookmarks";
      type = lib.types.listOf fileBookmark;
    };
  };
}