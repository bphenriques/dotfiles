{ config, lib, pkgs, osConfig, self, inputs, ... }:
let
  cfg = config.programs.quick-key;
  my = config.my.quick-key;
  niri = lib.getExe pkgs.niri;
  layoutKey = "Mod+K";
  keepAwakeKey = "Mod+I";

  # The top level, two columns paired by row; Calendar and Session sit in the bar.
  menuOrder = [ "audio" "network" "bluetooth" "display" "wallpaper" "tasks" "music" ];
  # The bar's right, left to right.
  barOrder = [ "mako" "ask" "keepAwake" "keyboardLayout" "battery" "tray" "session" ];

  placed = order: items: lib.concatMap (name: items.${name} or [ ]) order;
  unplaced = order: items: lib.filter (name: !(lib.elem name order)) (lib.attrNames items);
  named = description: lib.mkOption {
    type = lib.types.attrsOf (lib.types.listOf lib.types.anything);
    default = { };
    inherit description;
  };
in
{
  imports = [ inputs.quick-key.homeModules.default ];

  options.my.quick-key = {
    entries = named "quick-key's top-level entries by name, each module its own; menuOrder in quick-key.nix places them";
    bar = named "Items of quick-key's bar (its right) by name, each module its own; barOrder in quick-key.nix places them";
  };

  config = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    stylix.targets.quick-key.enable = true;

    assertions = [
      { assertion = unplaced menuOrder my.entries == [ ]; message = "my.quick-key.entries: ${toString (unplaced menuOrder my.entries)} not in menuOrder (quick-key.nix)"; }
      { assertion = unplaced barOrder my.bar == [ ]; message = "my.quick-key.bar: ${toString (unplaced barOrder my.bar)} not in barOrder (quick-key.nix)"; }
    ];

    programs.quick-key = {
      enable = true;

      settings = {
        theme.appIcons = "grey";
        theme.fontSize = 12;   # Hack at 12 reads as wide as a sans at 14 (Stylix popups)
        menu.keysHint = false;   # `?` still lists the keys
        battery = {
          low = osConfig.services.upower.percentageLow;
          critical = osConfig.services.upower.percentageCritical;
        };
        # The defaults, and the battery as it is plugged in or out; its warnings come through battery.warn.
        popups.show = [ "sink" "source" "brightness" "keyboardLayout" "keepAwake" "battery" ];
        keyboardLayout.labels = lib.map (layout: layout.label) config.my.programs.niri.keyboardLayouts;
        clock = {
          locale = "en_GB";
          format = "%A, %-d%o %B %H:%M";
        };
        plugins.niriMinimap = { };

        menu.entries = placed menuOrder my.entries;
        menu.header.right = placed barOrder my.bar;
      };
    };

    my.quick-key.entries.network = [
      { key = "n"; desc = "Network"; width = 640; status = "network"; list = "wifi"; submenu = [
        { key = "p"; desc = "Power"; action = "wifi"; }
        { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = config.my.programs.terminal.execApp { title = "nmtui-tui"; cmd = lib.getExe' pkgs.networkmanager "nmtui"; }; }
      ]; }
    ];
    my.quick-key.bar = {
      keepAwake = [ { key = "i"; desc = "Keep awake"; action = "keepAwake"; shortcut = keepAwakeKey; } ];
      keyboardLayout = [ { key = "k"; desc = "Keyboard layout"; status = "keyboardLayout"; action = "keyboardLayout"; shortcut = layoutKey; } ];
      battery = [ "battery" ];
      tray = [ { desc = "Tray"; list = "tray"; limit = 5; } ];
    };

    my.programs.niri.bindings = {
      "Mod+Ctrl+Space repeat=false" = ''spawn-sh "${cfg.exec.ipc} menu toggle"'';
      # A spawn, not the switch-layout action: only spawns run while locked.
      "${layoutKey} allow-when-locked=true" = ''spawn-sh "${niri} msg action switch-layout next"'';
      ${keepAwakeKey} = ''spawn-sh "${cfg.exec.ipc} keepAwake toggle"'';
    };
  };
}
