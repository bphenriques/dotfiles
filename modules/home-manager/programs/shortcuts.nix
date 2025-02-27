{ lib, pkgs, config, ... }:
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.shortcuts;

  fileBookmarkOpt = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      path = lib.mkOption { type = lib.types.str; };
    };
  };
in
{
  options.custom.programs.shortcuts = {
    enable = lib.mkEnableOption "shortcuts";
    files = {
      browser = lib.mkOption {
        description = "Package or executable to run that supports a single path argument";
        type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
      };

      bookmarks = lib.mkOption {
        description = "File bookmarks";
        type = lib.types.listOf fileBookmarkOpt;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.shortcuts" pkgs lib.platforms.linux) ];

    home.packages = [
      (pkgs.makeDesktopItem {
        name = "file-bookmarks";
        desktopName = "Open file bookmark";
        icon = "folder";  # FIXME
        exec = cfg.files.browser;
        actions = let
          bookmarkToAction = b: nameValuePair (replaceStrings [" "] ["-"] b.name) {
            inherit (b) name;
            exec = "${cfg.files.browser} ${b.path}";
          };
        in listToAttrs (lib.map bookmarkToAction cfg.files.bookmarks);
      })
    ];

    # Set some sane defaults
    custom.programs.shortcuts.files.bookmarks = [
      { name = "Documents";   path = config.xdg.userDirs.documents; }
      { name = "Pictures";    path = config.xdg.userDirs.pictures; }
      { name = "Music";       path = config.xdg.userDirs.music; }
      { name = "Downloads";   path = config.xdg.userDirs.download; }
    ];
  };
}
