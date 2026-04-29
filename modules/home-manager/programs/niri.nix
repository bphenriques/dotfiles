{ lib, pkgs, config, ... }:
# My biased module to manage Niri. There are other options out there but this one I prefer (avoids dependencies).
let
  cfg = config.custom.programs.niri;

  niriValidatedConfig = configText: pkgs.runCommandLocal "niri-config-validated" {
    nativeBuildInputs = [ pkgs.niri ];
    text = configText;
    passAsFile = [ "text" ];
  } ''
    niri validate -c "$textPath"
    cp "$textPath" $out
  '';

  displayOutputOpt = lib.types.submodule {
    options = {
      identifier  = lib.mkOption { type = lib.types.str; };
      resolution  = lib.mkOption { type = lib.types.str; };
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

    spawnAtStartup = lib.mkOption {
      description = "Programs to spawn at startup (path to binary, no arguments)";
      type = lib.types.listOf lib.types.str;
      default = [];
    };

    spawnShAtStartup = lib.mkOption {
      description = "Shell commands to run at startup (supports arguments, pipes, etc.)";
      type = lib.types.listOf lib.types.str;
      default = [];
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

        pip = lib.mkOption {
          description = "Rendered 3rd. Matching rules for picture-in-picture windows (overrides app defaults)";
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
        description = "Rendered 4th. Must-win rules (e.g., gaming opacity, urgent borders)";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      privacy = lib.mkOption {
        description = "Rendered 5th. Privacy rules (e.g., block-out-from screen-capture)";
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };

    layerRules = {
      launchers = lib.mkOption {
        description = "List of matching rules for launcher layers";
        type = lib.types.listOf lib.types.str;
        default = [];
      };

      screencasting = {
        block = lib.mkOption {
          description = "List of matching layer rules to block from screencasting";
          type = lib.types.listOf lib.types.str;
          default = [];
        };
      };

      extra = lib.mkOption {
        description = "Additional layer rules contributed by app profiles or global config";
        type = lib.types.listOf lib.types.str;
        default = [];
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
    xdg.configFile."niri/config.kdl".source = niriValidatedConfig ''
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

      ${lib.concatMapStringsSep "\n" (cmd: ''spawn-sh-at-startup "${cmd}"'') cfg.spawnShAtStartup}
      ${lib.concatMapStringsSep "\n" (entry: ''spawn-at-startup "${entry}"'') cfg.spawnAtStartup}

      // 1. Base defaults
      ${lib.concatStringsSep "\n\n" cfg.windowRules.base}

      // 2. App-specific rules
      ${lib.concatStringsSep "\n\n" cfg.windowRules.byApp}

      // 3. Window-kind overrides (byType) — override app defaults
      ${lib.optionalString (cfg.windowRules.byType.popups != []) ''
      window-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.byType.popups}

        open-floating true
        open-maximized false
        open-maximized-to-edges false
        open-fullscreen false
      }
      ''}

      ${lib.optionalString (cfg.windowRules.byType.pip != []) ''
      window-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.windowRules.byType.pip}

        open-floating true
        open-focused false
        open-maximized false
        open-maximized-to-edges false
        open-fullscreen false
        default-column-width { fixed 480; }
        default-window-height { fixed 270; }
        default-floating-position x=32 y=32 relative-to="bottom-right"
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

      // 5. Privacy rules
      ${lib.concatStringsSep "\n\n" cfg.windowRules.privacy}

      ${lib.optionalString (cfg.layerRules.launchers != []) ''
      layer-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.layerRules.launchers}
        shadow {
          on
        }

        geometry-corner-radius 10

        background-effect {
          blur true
        }
      }
      ''}

      ${lib.concatStringsSep "\n\n" cfg.layerRules.extra}

      ${lib.optionalString (cfg.layerRules.screencasting.block != []) ''
      layer-rule {
        ${lib.strings.concatMapStringsSep "\n" (match: ''match ${match}'') cfg.layerRules.screencasting.block}
        block-out-from "screencast"
      }
      ''}

      // Keyboard-triggered actions use short fixed durations (easing).
      // Gesture-sensitive actions (touchpad swipes) use springs to respond to finger velocity.
      // Springs: higher stiffness = snappier. damping-ratio=1.0 = no oscillation (critically damped).
      animations {
        window-open {
          duration-ms 150
        }
        window-close {
          duration-ms 150
        }
        window-resize {
          duration-ms 150
        }
        window-movement {
          duration-ms 150
        }
        workspace-switch {
          spring damping-ratio=1.0 stiffness=1200 epsilon=0.0001
        }
        horizontal-view-movement {
          spring damping-ratio=1.0 stiffness=1000 epsilon=0.0001
        }
        overview-open-close {
          spring damping-ratio=1.0 stiffness=1000 epsilon=0.0001
        }
      }

      binds {
        ${lib.strings.concatStringsSep "\n" (lib.mapAttrsToList (binding: action: ''${binding} { ${action}; }'') cfg.bindings)}
      }

      ${cfg.extraConfig}
    '';
  };
}
