{ lib, pkgs, config, self, ... }:

let
  cfg = config.custom.desktop-environment;

  mkRunOption = description: lib.mkOption {
    inherit description;
    type = lib.types.str;
  };

  # TODO: Each entry has optional package and a exec argument. Exec argument defaults to lib.getExe of the package.
  # TODO: Opt. add a Niri spawn override.
in
{
  options.custom.desktop-environment = {
    application-launcher = mkRunOption "Application launcher";
    emoji-picker = mkRunOption "Emoji Picker";
    window-switcher = mkRunOption "Window switcher";
    session-menu = mkRunOption "Session Menu";
    terminal = mkRunOption "Terminal launcher";
    tui = mkRunOption "Terminal TUI launcher";
    system-monitor = mkRunOption "System Monitor";
  };
}
