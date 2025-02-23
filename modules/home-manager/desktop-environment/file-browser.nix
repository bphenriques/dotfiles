{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;
  inherit (config.custom.desktop-environment) terminal;

  cfg = config.custom.desktop-environment.files;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  fileBookmarkOpt = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      path = lib.mkOption { type = lib.types.str; };
    };
  };
in
{
  options.custom.desktop-environment.files = {
    browser = mkAppOpt "${terminal.emulator} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";

    bookmarks = lib.mkOption {
      description = "File bookmarks";
      type = lib.types.listOf fileBookmarkOpt;
    };
  };

  config = {
    home.packages = [
      (pkgs.makeDesktopItem {
        name = "File Browser";
        desktopName = "Open file browser";
        icon = "folder";  # FIXME
        exec = cfg.browser;
        actions = let
          bookmarkToAction = b: nameValuePair (replaceStrings [" "] ["-"] b.name) {
            inherit (b) name;
            exec = "${cfg.browser} ${b.path}";
          };
        in listToAttrs (lib.map bookmarkToAction cfg.bookmarks);
      })
    ];
  };
}
