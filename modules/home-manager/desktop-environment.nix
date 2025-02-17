{ lib, pkgs, config, self, ... }:

let
  cfg = config.custom.desktop-environment;

  desktopOption = lib.types.submodule {
    options = {
      package = lib.mkOption { type = lib.types.package; }; #
      exe = lib.mkOption { type = lib.types.package; };     # default to lib.getExe
      # either one or another must be set
    };
  };

  mkRunOption = description: lib.mkOption {
    inherit description;
    type = lib.types.str;
  };

  # TODO: Each entry has optional package and a exec argument. Exec argument defaults to lib.getExe of the package.
  # TODO: Opt. add a Niri spawn override.
in
{
  options.custom.desktop-environment = {
    application-launcher  = mkRunOption "Application launcher";
    dmenu                 = mkRunOption "Emoji Picker";
    emoji-picker          = mkRunOption "Dmenu runner";
    window-switcher       = mkRunOption "Window switcher";
    session-menu          = mkRunOption "Session Menu";
    terminal              = mkRunOption "Terminal launcher";
    tui                   = mkRunOption "Terminal TUI launcher";
    system-monitor        = mkRunOption "System Monitor";
    screen-lock           = mkRunOption "Screen Lock";
    screenshot-menu       = mkRunOption "Screenshot menu";
    file-browser          = mkRunOption "Screenshot menu";
  };
}
