{ lib, pkgs, config, self, ... }:
let
  inherit (lib) mkOption types;

  cfg = config.my.programs.desktop-shell;
  settingsFormat = pkgs.formats.json { };
  settingsFile = settingsFormat.generate "desktop-shell-config.json" cfg.settings;

  entry = types.submodule {
    options = {
      key = mkOption {
        type = types.either types.str (types.listOf types.str);
        description = "Key or keys that trigger the entry, as xkb key names without modifiers (e.g. `a`, `A`, `Left`, `space`)";
      };
      desc = mkOption {
        type = types.str;
        description = "Label shown next to the key";
      };
      cmd = mkOption {
        type = types.nullOr (types.either types.str (types.listOf types.str));
        default = null;
        description = "Command to run, or one per key for a pair (e.g. Down and Up) shown as one row";
      };
      keepOpen = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = "Keep the menu open after running cmd";
      };
      activeWhen = mkOption {
        type = types.nullOr (types.enum [ "wifi" "bluetooth" "repeat" "shuffle" "sink-muted" "source-muted" ]);
        default = null;
        description = "State that lights the entry while on, for a toggle";
      };
      layout = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Display layout the entry selects, lit while current and dim while the connected screens do not allow it";
      };
      width = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Width of this entry's level; menu.width otherwise";
      };
      columns = mkOption {
        type = types.nullOr types.ints.positive;
        default = null;
        description = "Columns of this entry's level, entries flowing across; menu.columns otherwise";
      };
      submenu = mkOption {
        type = types.nullOr (types.listOf entry);
        default = null;
        description = "Nested entries";
      };
      list = mkOption {
        type = types.nullOr (types.enum [ "tray" "bluetooth" "wifi" "sinks" "sources" ]);
        default = null;
        description = "Nested entries filled at runtime (tray apps, paired Bluetooth devices, saved Wi-Fi networks in range, audio outputs or inputs), numbered after any submenu";
      };
      pins = mkOption {
        type = types.listOf (types.submodule {
          options = {
            match = mkOption {
              type = types.str;
              description = "Pattern, with * for anything, on the item's id: audio node name, Bluetooth address, Wi-Fi name or tray app id";
            };
            label = mkOption {
              type = types.str;
              description = "Name shown for the item, also while it is absent";
            };
            icon = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Glyph shown for the item, the same present or absent";
            };
          };
        });
        default = [ ];
        description = "Items of the list that keep digits 1 onwards, dimmed while absent";
      };
      status = mkOption {
        type = types.nullOr (types.enum [ "sink" "source" "music" "brightness" "network" "bluetooth" "display" "tray" "wallpaper" ]);
        default = null;
        description = "Status shown in place of the label";
      };
      requires = {
        connected = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Outputs that must be connected for the entry to be enabled: connector name, or make, model and serial";
        };
        enabled = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Outputs that must also be on, e.g. the panel a brightness entry dims";
        };
      };
      icon = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Glyph shown before the label: on every action in a level, and on a top-level entry without status";
      };
    };
  };

  # Esc closes the menu and Backspace goes back a level.
  reservedKeys = [ "Escape" "BackSpace" ];

  digits = map toString (lib.range 0 9);

  keysOf = entries: lib.concatMap (e: lib.toList e.key) entries;

  levelAssertions = path: entries:
    let
      keys = keysOf entries;
      duplicates = lib.unique (lib.filter (k: lib.count (x: x == k) keys > 1) keys);
      withModifiers = lib.filter (k: k != "+" && lib.hasInfix "+" k) keys;
      reserved = lib.intersectLists reservedKeys keys;
    in [
      { assertion = duplicates == [ ]; message = "my.programs.desktop-shell.menu: duplicate keys in ${path}: ${toString duplicates}"; }
      { assertion = withModifiers == [ ]; message = "my.programs.desktop-shell.menu: modifiers are not supported in ${path}: ${toString withModifiers}"; }
      { assertion = reserved == [ ]; message = "my.programs.desktop-shell.menu: ${toString reserved} in ${path} are reserved for the menu"; }
    ]
    ++ lib.concatMap (e: [
      { assertion = (e.cmd != null) != (e.submenu != null || e.list != null); message = "my.programs.desktop-shell.menu: '${e.desc}' in ${path} needs either cmd, or submenu and/or list"; }
      { assertion = e.keepOpen == null || e.cmd != null; message = "my.programs.desktop-shell.menu: '${e.desc}' in ${path} can only set keepOpen with cmd"; }
      { assertion = e.submenu == null || lib.all (a: a.icon != null) e.submenu; message = "my.programs.desktop-shell.menu: every action in '${e.desc}' needs an icon"; }
      { assertion = e.pins == [ ] || e.list != null; message = "my.programs.desktop-shell.menu: '${e.desc}' in ${path} can only set pins with list"; }
      { assertion = lib.length e.pins <= 9; message = "my.programs.desktop-shell.menu: '${e.desc}' in ${path} has more pins than digits"; }
      { assertion = e.list == null || lib.intersectLists digits (keysOf (if e.submenu != null then e.submenu else [ ])) == [ ]; message = "my.programs.desktop-shell.menu: digits in '${e.desc}' are reserved for its list"; }
      { assertion = !(lib.isList e.cmd) || (lib.isList e.key && lib.length e.cmd == lib.length e.key); message = "my.programs.desktop-shell.menu: '${e.desc}' in ${path} pairs keys and commands, so needs as many of each"; }
    ]) entries
    ++ lib.concatMap (e: lib.optionals (e.submenu != null) (levelAssertions "${path} > ${e.desc}" e.submenu)) entries;
in
{
  options.my.programs.desktop-shell = {
    enable = lib.mkEnableOption "desktop-shell, a keyboard-driven Quickshell menu with live status";

    package = mkOption {
      type = types.package;
      default = self.packages.desktop-shell;
      description = "Shell package, its QML under share/desktop-shell";
    };

    settings = mkOption {
      type = settingsFormat.type;
      default = { };
      description = "Written to $XDG_CONFIG_HOME/desktop-shell/config.json, the shell's only input from Nix";
    };

    menu.entries = mkOption {
      type = types.listOf entry;
      default = [ ];
      description = "Menu entries";
    };

    exec.ipc = mkOption {
      type = types.str;
      default = "${lib.getExe config.programs.quickshell.package} -c desktop-shell ipc call";
      description = "Prefix for the shell's IPC calls; append the target and function, e.g. `menu toggle`";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "my.programs.desktop-shell" pkgs lib.platforms.linux) ]
      ++ levelAssertions "the top level" cfg.menu.entries;

    my.programs.desktop-shell.settings.menu = { inherit (cfg.menu) entries; };

    xdg.configFile."desktop-shell/config.json".source = settingsFile;

    programs.quickshell = {
      enable = true;
      systemd.enable = true;
      activeConfig = "desktop-shell";
      configs.desktop-shell = "${cfg.package}/share/desktop-shell";
    };

    # home-manager swaps the settings symlink on switch, which a file watch would not follow; restart instead.
    systemd.user.services.quickshell.Unit = {
      PartOf = [ config.wayland.systemd.target ];
      X-Restart-Triggers = [ "${cfg.package}" "${settingsFile}" ];
    };
  };
}
