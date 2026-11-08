{ config, lib, pkgs, osConfig, ... }:
let
  rfkill = lib.getExe' pkgs.util-linux "rfkill";
in
lib.mkIf (pkgs.stdenv.hostPlatform.isLinux && osConfig.hardware.bluetooth.enable) {
  my.programs.desktop-shell.menu.entries = lib.mkOrder 400 [
    { key = "b"; desc = "Bluetooth"; width = 640; status = "bluetooth"; list = "bluetooth"; submenu = [
      { key = "p";     desc = "Power";      activeWhen = "bluetooth"; cmd = "${rfkill} toggle bluetooth"; keepOpen = true; }
      { key = "c";     desc = "Configure..."; cmd = config.my.programs.terminal.execApp { title = "bluetui-tui"; cmd = lib.getExe pkgs.bluetui; }; }
    ]; }
  ];

  my.programs.niri.windowRules.byType.tui = [ ''title="bluetui-tui"'' ];
}
