{ lib, config, ... }:
# niri resolves rule precedence by position, so these staged options fix it. Rules sharing a stage must not overlap.
let
  cfg = config.my.programs.niri;

  startupNodes = lib.map (cmd: { spawn-sh-at-startup._args = [ cmd ]; }) cfg.spawnShAtStartup;
in
{
  options.my.programs.niri = {
    enable = lib.mkEnableOption "programs-niri";

    spawnShAtStartup = lib.mkOption {
      description = "Shell commands to run at startup (supports arguments, pipes, etc.)";
      type = lib.types.listOf lib.types.str;
      default = [];
    };

    bindings = lib.mkOption {
      description = "Key value set between a key combination and the respective action";
      type = lib.types.attrsOf lib.types.str;
    };

    windowRules = {
      base = lib.mkOption {
        description = "Rendered 1st: generic defaults";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      byApp = lib.mkOption {
        description = "Rendered 2nd: per-app rules";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      byType = {
        popups = lib.mkOption {
          description = "Rendered 3rd: popup rules";
          type = lib.types.listOf lib.types.str;
          default = [];
        };

        tui = lib.mkOption {
          description = "Rendered 3rd: TUI app rules";
          type = lib.types.listOf lib.types.str;
          default = [];
        };
      };

      overrides = lib.mkOption {
        description = "Rendered last: must-win rules";
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };

    layerRules = {
      base = lib.mkOption {
        description = "Rendered 1st: per-app layer rules";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      screencasting.block = lib.mkOption {
        description = "Rendered 2nd: layers blocked from screencasting";
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    wayland.systemd.target = "niri.service";

    wayland.windowManager.niri = {
      enable = true;
      systemd.enable = false; # units and portals come from the NixOS `programs.niri` module
      portalPackage = null;

      settings._children = startupNodes;

      extraConfig = ''
        // 1. Base defaults
        ${lib.concatStringsSep "\n\n" cfg.windowRules.base}

        // 2. App-specific rules
        ${lib.concatStringsSep "\n\n" cfg.windowRules.byApp}

        // 3. Window-kind overrides (byType). Override app defaults
        ${lib.optionalString (cfg.windowRules.byType.popups != []) ''
        window-rule {
          ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.byType.popups}

          open-floating true
          open-maximized false
          open-maximized-to-edges false
          open-fullscreen false
        }
        ''}

        ${lib.optionalString (cfg.windowRules.byType.tui != []) ''
        window-rule {
          ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.byType.tui}

          open-floating true
          open-maximized false
          open-maximized-to-edges false
          open-fullscreen false
          default-column-width { fixed 1280; }
          default-window-height { fixed 720; }
        }
        ''}

        // 4. Must-win overrides
        ${lib.concatStringsSep "\n\n" cfg.windowRules.overrides}

        // 5. Layer rules
        ${lib.concatStringsSep "\n\n" cfg.layerRules.base}

        ${lib.optionalString (cfg.layerRules.screencasting.block != []) ''
        layer-rule {
          ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.layerRules.screencasting.block}
          block-out-from "screencast"
        }
        ''}

        binds {
          ${lib.strings.concatStringsSep "\n" (lib.mapAttrsToList (binding: action: ''${binding} { ${action}; }'') cfg.bindings)}
        }
      '';
    };
  };
}
