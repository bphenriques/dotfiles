{ config, lib, pkgs, osConfig, self, inputs, ... }:
let
  cfg = config.programs.quick-key;
  colors = config.lib.stylix.colors.withHashtag;
  niri = lib.getExe pkgs.niri;
  layoutKey = "Mod+K";
  keepAwakeKey = "Mod+I";
in
{
  imports = [ inputs.quick-key.homeModules.default ];

  config = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    programs.quick-key = {
      enable = true;

      themes.stylix = {
        background = colors.base00;
        foreground = colors.base05;
        primary = colors.base0D;
        alert = colors.base08;
        warning = colors.base0A;
      };

      settings = {
        theme = {
          name = "stylix";
          monospaceFont = config.stylix.fonts.monospace.name;
          fontSize = config.stylix.fonts.sizes.popups;
        };
        battery = {
          low = osConfig.services.upower.percentageLow;
          critical = osConfig.services.upower.percentageCritical;
        };
        keyboardLayout = {
          labels = lib.map (layout: layout.label) config.my.programs.niri.keyboardLayouts;
          shortcut = layoutKey;
        };
        clock = {
          locale = "en_GB";
          format = "%A, %-d%o %B %H:%M";
        };
        keepAwake.shortcut = keepAwakeKey;
        menu.header.right = [ "keepAwake" "keyboardLayout" "battery" ];

        plugins.niriMinimap = { };

        # Top level: 100 audio, 300 network, 400 Bluetooth, 500 display, 600 wallpaper, 700 music, 800 calendar, 900 tray, 2000 session.
        menu.entries = lib.mkMerge [
          (lib.mkOrder 300 [
            { key = "n"; desc = "Network"; width = 640; status = "network"; list = "wifi"; submenu = [
              { key = "p"; desc = "Power"; action = "wifi"; }
              { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = config.my.programs.terminal.execApp { title = "nmtui-tui"; cmd = lib.getExe' pkgs.networkmanager "nmtui"; }; }
            ]; }
          ])
          (lib.mkOrder 900 [
            { key = "t"; desc = "Tray"; width = 640; status = "tray"; list = "tray"; }
          ])
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
