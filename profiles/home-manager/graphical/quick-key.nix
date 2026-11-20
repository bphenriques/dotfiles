{ config, lib, pkgs, osConfig, self, inputs, ... }:
let
  cfg = config.programs.quick-key;
  niri = lib.getExe pkgs.niri;
  layoutKey = "Mod+K";
  keepAwakeKey = "Mod+I";
in
{
  imports = [ inputs.quick-key.homeModules.default ];

  config = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    stylix.targets.quick-key.enable = true;

    programs.quick-key = {
      enable = true;

      settings = {
        theme.monochromeTray = true;
        battery = {
          low = osConfig.services.upower.percentageLow;
          critical = osConfig.services.upower.percentageCritical;
        };
        keyboardLayout.labels = lib.map (layout: layout.label) config.my.programs.niri.keyboardLayouts;
        clock = {
          locale = "en_GB";
          format = "%A, %-d%o %B %H:%M";
        };
        menu.header.right = [
          { key = "i"; desc = "Keep awake"; action = "keepAwake"; shortcut = keepAwakeKey; }
          { key = "k"; desc = "Keyboard layout"; status = "keyboardLayout"; action = "keyboardLayout"; shortcut = layoutKey; }
          "battery"
          { desc = "Tray"; list = "tray"; limit = 5; }
        ];

        plugins.niriMinimap = { };

        # Top level, paired by row: 100 audio, 300 network, 400 Bluetooth, 500 display, 600 wallpaper, 800 tasks, 1000 music. Calendar and Session sit in the bar.
        menu.entries = lib.mkOrder 300 [
          { key = "n"; desc = "Network"; width = 640; status = "network"; list = "wifi"; submenu = [
            { key = "p"; desc = "Power"; action = "wifi"; }
            { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = config.my.programs.terminal.execApp { title = "nmtui-tui"; cmd = lib.getExe' pkgs.networkmanager "nmtui"; }; }
          ]; }
        ];
      };
    };

    my.programs.niri.bindings = {
      "Mod+Ctrl+Space repeat=false" = ''spawn-sh "${cfg.exec.ipc} menu toggle"'';
      # A spawn, not the switch-layout action: only spawns run while locked.
      "${layoutKey} allow-when-locked=true" = ''spawn-sh "${niri} msg action switch-layout next"'';
      ${keepAwakeKey} = ''spawn-sh "${cfg.exec.ipc} idle toggle"'';
    };
  };
}
