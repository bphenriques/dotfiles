{ lib, pkgs, config, ... }:
# My biased module to manage Niri. There are other options out there but this one I prefer (avoids dependencies).
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.niri;

  displayOutputOpt = lib.types.submodule {
    options = {
      identifier  = lib.mkOption { type = lib.types.str; };
      resolution  = lib.mkOption { type = lib.types.str; };
      position    = lib.mkOption { type = lib.types.nullOr lib.types.str; default = null; };
      refreshRate = lib.mkOption { type = lib.types.str; };
      scale       = lib.mkOption { type = lib.types.str; };
    };
  };
in
{
  options.custom.programs.niri = {
    enable = lib.mkEnableOption "programs-niri";

    environment = lib.mkOption {
      description = "Environment variables to set in a niri session";
      type = lib.types.attrsOf lib.types.str;
      default = {};
    };

    screenshotPath = lib.mkOption {
      description = "Path to screenshots";
      type = lib.types.str;
    };

    input = {
      keyboard = {
        xkb = {
          layout = lib.mkOption {
            description = "xkb layout settings";
            type = lib.types.str;
          };

          variant = lib.mkOption {
            description = "xkb variant settings";
            type = lib.types.str;
          };

          options = lib.mkOption {
            description = "xkb layout settings";
            type = lib.types.str;
          };
        };
        extraOptions = lib.mkOption {
          description = "Keyboard settings";
          type = lib.types.listOf lib.types.str;
          default = [];
        };
      };

      touchpad = lib.mkOption {
        description = "Touchpad settings";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      mouse = lib.mkOption {
        description = "Mouse settings";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      extraOptions = lib.mkOption {
        description = "Extra input options";
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };

    windowRules = {
      popups = lib.mkOption {
        description = "List of matching rules for popups windows";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      pip = lib.mkOption {
        description = "List of matching rules for picture-in-picture windows";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      tui = lib.mkOption {
        description = "List of matching rules for terminal user interface applications";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      screencasting = {
        block = lib.mkOption {
          description = "List of matching window rules to block from screencasting";
          type = lib.types.listOf lib.types.str;
          default = [];
        };
      };
    };

    layerRules = {
      launchers = lib.mkOption {
        description = "List of matching rules for launcher layers";
        type = lib.types.listOf lib.types.str;
      };

      screencasting = {
        block = lib.mkOption {
          description = "List of matching layer rules to block from screencasting";
          type = lib.types.listOf lib.types.str;
          default = [];
        };
      };
    };

    layout = lib.mkOption {
      description = "Layout configuration";
      type = lib.types.lines;
    };

    output = {
      default = lib.mkOption {
        description = "Default display output";
        type = displayOutputOpt;
      };
    };

    bindings = lib.mkOption {
      description = "Key value set between a key combination and the respective action";
      type = lib.types.attrsOf lib.types.str;
    };

    extraConfig = lib.mkOption {
      description = "Extra config to be added to the end";
      type = lib.types.lines;
      default = "";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.niri" pkgs lib.platforms.linux) ];

    wayland.systemd.target = "niri.service";
    xdg.configFile."niri/config.kdl".text = ''
      prefer-no-csd
      screenshot-path "${cfg.screenshotPath}"
      hotkey-overlay {
        skip-at-startup
      }

      environment {
        ${lib.strings.concatStringsSep "\n" (lib.mapAttrsToList (key: value: ''${key} "${value}"'') cfg.environment)}
      }

      input {
        keyboard {
          xkb {
            layout "${cfg.input.keyboard.xkb.layout}"
            variant "${cfg.input.keyboard.xkb.variant}"
            options "${cfg.input.keyboard.xkb.options}"
          }
          ${lib.strings.concatStringsSep "\n" cfg.input.keyboard.extraOptions}
        }

        touchpad {
          ${lib.strings.concatStringsSep "\n" cfg.input.touchpad}
        }

        mouse {
          ${lib.strings.concatStringsSep "\n" cfg.input.mouse}
        }

        ${lib.strings.concatStringsSep "\n" cfg.input.extraOptions}
      };

      output "${cfg.output.default.identifier}" {
        mode "${cfg.output.default.resolution}@${cfg.output.default.refreshRate}"
        scale ${cfg.output.default.scale}
      }

      layout {
        ${cfg.layout}
      }

      window-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.popups}

        open-floating true
      }

      window-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.pip}

        open-floating true
        open-focused false
        default-column-width { fixed 480; }
        default-window-height { fixed 270; }
        default-floating-position x=32 y=32 relative-to="bottom-right"
      }

      window-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.tui}

        default-column-width { fixed 1280; }
        default-window-height { fixed 720; }

        open-floating true
      }

      window-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.screencasting.block}
        block-out-from "screencast"
      }

      layer-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.layerRules.launchers}
        shadow {
          on
        }

        geometry-corner-radius 10
      }

      layer-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.layerRules.screencasting.block}
        block-out-from "screencast"
      }

      // Indicate screencasted windows with red colors.
      window-rule {
        match is-window-cast-target=true

        focus-ring {
          active-color "#f38ba8"
          inactive-color "#7d0d2d"
        }

        border {
          inactive-color "#7d0d2d"
        }

        shadow {
          color "#7d0d2d70"
        }

        tab-indicator {
          active-color "#f38ba8"
          inactive-color "#7d0d2d"
        }
      }

      animations {
      }

      binds {
        ${lib.strings.concatStringsSep "\n" (lib.mapAttrsToList (binding: action: ''${binding} { ${action}; }'') cfg.bindings)}
      }

      ${cfg.extraConfig}
    '';
  };
}
