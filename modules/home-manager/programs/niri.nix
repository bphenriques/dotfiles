{ lib, config, ... }:
# niri resolves rule precedence positionally, so the staged options below fix each rule's
# position; Nix merge order is not visible to contributors. Rules sharing a stage must not
# match the same window or layer. Keep this the only writer of upstream `extraConfig`.
let
  cfg = config.custom.programs.niri;

  workspaceOpt = lib.types.submodule ({ name, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Niri workspace name (defaults to the attribute key)";
      };
      order = lib.mkOption {
        type = lib.types.int;
        description = "Position in niri's named-workspace list; must be unique";
      };
      openOnDefaultOutput = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Pin workspace to the default output so it follows monitor switches";
      };
    };
  });

  displayOutputOpt = lib.types.submodule {
    options = {
      identifier  = lib.mkOption { type = lib.types.str; };
      resolution  = lib.mkOption { type = lib.types.str; };
      refreshRate = lib.mkOption { type = lib.types.str; };
      scale       = lib.mkOption { type = lib.types.float; };
    };
  };

  workspaces = lib.sort (a: b: a.order < b.order) (lib.attrValues cfg.workspaces);

  workspaceNodes = lib.map (ws: {
    workspace = { _args = [ ws.name ]; }
      // lib.optionalAttrs ws.openOnDefaultOutput { open-on-output = cfg.output.default.identifier; };
  }) workspaces;

  startupNodes = lib.map (cmd: { spawn-sh-at-startup._args = [ cmd ]; }) cfg.spawnShAtStartup;
in
{
  options.custom.programs.niri = {
    enable = lib.mkEnableOption "programs-niri";

    workspaces = lib.mkOption {
      description = "Named workspaces; attribute keys are stable identifiers, .name is the niri-visible label";
      type = lib.types.attrsOf workspaceOpt;
      default = {};
    };

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
        description = "Rendered 1st. Generic defaults (corner-radius, opacity, floating shadow)";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      byApp = lib.mkOption {
        description = "Rendered 2nd. App-specific rules contributed by individual app profiles";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      byType = {
        popups = lib.mkOption {
          description = "Rendered 3rd. Matching rules for popup windows (overrides app defaults)";
          type = lib.types.listOf lib.types.str;
          default = [];
        };

        tui = lib.mkOption {
          description = "Rendered 3rd. Matching rules for terminal user interface applications (overrides app defaults)";
          type = lib.types.listOf lib.types.str;
          default = [];
        };
      };

      overrides = lib.mkOption {
        description = "Rendered last. Must-win rules (e.g., gaming opacity, urgent borders)";
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };

    layerRules = {
      base = lib.mkOption {
        description = "Rendered 1st. Layer rules contributed by individual app profiles";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      screencasting.block = lib.mkOption {
        description = "Rendered 2nd. Matching layer rules to block from screencasting";
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };

    output = {
      default = lib.mkOption {
        description = "Default display output";
        type = displayOutputOpt;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.length (lib.unique (lib.map (ws: ws.order) workspaces)) == lib.length workspaces;
        message = "custom.programs.niri.workspaces: `order` must be unique";
      }
    ];

    wayland.systemd.target = "niri.service";

    wayland.windowManager.niri = {
      enable = true;
      systemd.enable = false; # units and portals come from the NixOS `programs.niri` module
      portalPackage = null;

      settings = {
        output = {
          _args = [ cfg.output.default.identifier ];
          mode = "${cfg.output.default.resolution}@${cfg.output.default.refreshRate}";
          inherit (cfg.output.default) scale;
        };

        _children = workspaceNodes ++ startupNodes;
      };

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
