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
        type = types.nullOr types.str;
        default = null;
        description = "Command to run";
      };
      keepOpen = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = "Keep the menu open after running cmd";
      };
      submenu = mkOption {
        type = types.nullOr (types.listOf entry);
        default = null;
        description = "Nested entries";
      };
      list = mkOption {
        type = types.nullOr (types.enum [ "tray" ]);
        default = null;
        description = "Nested entries filled at runtime, e.g. tray apps";
      };
      status = mkOption {
        type = types.nullOr (types.enum [ "sink" "source" "music" "brightness" "network" "bluetooth" "display" "tray" "wallpaper" ]);
        default = null;
        description = "Status shown in place of the label";
      };
      icon = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Glyph shown before the label of an entry without status";
      };
    };
  };

  # Esc closes the menu and Backspace goes back a level.
  reservedKeys = [ "Escape" "BackSpace" ];

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
      { assertion = lib.count (x: x != null) [ e.cmd e.submenu e.list ] == 1; message = "my.programs.desktop-shell.menu: '${e.desc}' in ${path} needs exactly one of cmd, submenu or list"; }
      { assertion = e.keepOpen == null || e.cmd != null; message = "my.programs.desktop-shell.menu: '${e.desc}' in ${path} can only set keepOpen with cmd"; }
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
      description = "Menu entries, laid out and triggered like wlr-which-key";
    };

    exec.menu = mkOption {
      type = types.str;
      default = "${lib.getExe config.programs.quickshell.package} -c desktop-shell ipc call menu toggle";
      description = "Command that opens or closes the menu";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "my.programs.desktop-shell" pkgs lib.platforms.linux) ]
      ++ levelAssertions "the top level" cfg.menu.entries;

    my.programs.desktop-shell.settings = {
      theme = lib.mapAttrs (_: lib.mkDefault) {
        surfaceOpacity = 0.92;
        highlightOpacity = 0.25;
        dimOpacity = 0.5;
        radius = 10;
        itemRadius = 6;
        iconSize = 16;
        borderWidth = 1;
        spacing = 8;
        padding = 12;
        itemPaddingX = 8;
        itemPaddingY = 4;
        listSpacing = 0;
        edgeMargin = 12;
      };
      menu = {
        inherit (cfg.menu) entries;
        width = lib.mkDefault 720;
        rowsPerColumn = lib.mkDefault 5;
      };
    };

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
