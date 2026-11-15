{ config, lib, pkgs, osConfig, self, ... }:
lib.mkIf (pkgs.stdenv.hostPlatform.isLinux && osConfig.hardware.bluetooth.enable) {
  programs.quick-key.settings.menu.entries = lib.mkOrder 400 [
    { key = "b"; desc = "Bluetooth"; width = 640; status = "bluetooth"; list = "bluetooth"; submenu = [
      { key = "p"; desc = "Power"; action = "bluetooth"; }
      { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = config.my.programs.terminal.execApp { title = "bluetui-tui"; cmd = lib.getExe pkgs.bluetui; }; }
    ]; }
  ];

  my.programs.niri.windowRules.byType.tui = [ ''title="bluetui-tui"'' ];
}
