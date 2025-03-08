{ lib, pkgs, config, self, ... }:
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.file-explorer;

  fileBookmarkOpt = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      path = lib.mkOption { type = lib.types.str; };
      icon = lib.mkOption { type = lib.types.nullOr lib.types.str; default = null; };
    };
  };

  mkIcon = name: symbol: self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } name symbol;
in
{
  options.custom.programs.file-explorer = {
    enable = lib.mkEnableOption "file-explorer";
    browser = lib.mkOption {
      description = "Package or executable to run that supports a single path argument";
      type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
    };

    bookmarks = lib.mkOption {
      description = "File bookmarks";
      type = lib.types.listOf fileBookmarkOpt;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.file-explorer" pkgs lib.platforms.linux) ];

    home.packages = [
      (pkgs.makeDesktopItem {
        name = "file-explorer";
        desktopName = "File Explorer";
        icon = mkIcon "file-explorer" "";
        exec = cfg.browser;
        actions = let
          bookmarkToAction = b: nameValuePair (replaceStrings [" "] ["-"] b.name) {
            inherit (b) name icon;
            exec = "${cfg.browser} ${b.path}";
          };
        in listToAttrs (lib.map bookmarkToAction cfg.bookmarks);
      })
    ];
    custom.xdgDefaultApps.fileBrowser = lib.mkBefore [ "file-explorer.desktop" ];

    # Set some sane defaults
    custom.programs.file-explorer.bookmarks = [
      {
        name = "Documents";
        icon = mkIcon "documents" "󱧶";
        path = config.xdg.userDirs.documents;
      }
      {
        name = "Pictures";
        icon = mkIcon "pictures" "󰉏";
        path = config.xdg.userDirs.pictures;
      }
      {
        name = "Music";
        icon = mkIcon "music" "󱍙";
        path = config.xdg.userDirs.music;
      }
      {
        name = "Downloads";
        icon = mkIcon "downloads" "󰉍";
        path = config.xdg.userDirs.download;
      }
    ];
  };
}
