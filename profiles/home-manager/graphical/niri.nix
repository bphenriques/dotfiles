{ config, lib, pkgs, ... }:
let
  inherit (config.custom.programs) screenshot;
  colors = config.lib.stylix.colors.withHashtag;

  volume          = lib.getExe config.custom.programs.volume-osd.package;
  brightness      = lib.getExe config.custom.programs.brightness-osd.package;
  terminal        = config.custom.programs.terminal.exec;
  playerctl       = lib.getExe pkgs.playerctl;
  dmenu           = "${lib.getExe config.programs.fuzzel.package} -d";

  emoji = pkgs.writeShellApplication {
    name = "emoji-picker";
    runtimeInputs = [ pkgs.wtype ];
    text = ''BEMOJI_PICKER_CMD="${dmenu}" ${lib.getExe pkgs.bemoji} --noline --type --clip'';
  };
in
{
  custom.programs.niri = {
    enable = true;

    # Pinned to the default output so niri moves them to the active monitor when kanshi disables it.
    workspaces = {
      browser = { order = 1; openOnDefaultOutput = true; };
      main    = { order = 2; openOnDefaultOutput = true; };
      gaming  = { order = 3; openOnDefaultOutput = true; };
    };

    windowRules = {
      byType = {
        popups = lib.map (title: ''title="${title}"'') [
          "^(pwvucontrol)"
          "^(Volume Control)"
          "^(dialog)"
          "^(file_progress)"
          "^(confirm)"
          "^(download)"
          "^(error)"
          "^(notification)"
        ];

        tui = lib.map (title: ''title="${title}"'') [
          "nmtui-tui"
        ];
      };

      base = [
        ''
          window-rule {
            match is-floating=true
            shadow {
              softness 30
              spread 5
            }
          }
        ''
        ''
          window-rule {
            geometry-corner-radius 6
            clip-to-geometry true

            popups {
              geometry-corner-radius 15
            }
          }
        ''
        ''
          window-rule {
            match is-active=false
            opacity 0.90
          }
        ''
      ];

      # Generic overrides. Cross-cutting rules not tied to any app profile
      overrides = [
        ''
          window-rule {
            match title="^Picture in picture$"
            match title="^Discord Popout$"

            open-floating true
            open-focused false
            open-maximized false
            open-maximized-to-edges false
            open-fullscreen false
            default-column-width { fixed 480; }
            default-window-height { fixed 270; }
            default-floating-position x=32 y=32 relative-to="bottom-right"
          }
        ''
        ''
          window-rule {
            match is-urgent=true
            border {
              on
              active-color "${colors.base08}"
              inactive-color "${colors.base08}"
            }
          }
        ''
        ''
          window-rule {
            match is-window-cast-target=true

            focus-ring {
              active-color "${colors.base08}"
              inactive-color "${colors.base01}"
            }

            border {
              inactive-color "${colors.base01}"
            }

            shadow {
              color "${colors.base08}70"
            }

            tab-indicator {
              active-color "${colors.base08}"
              inactive-color "${colors.base01}"
            }
          }
        ''
      ];
    };

    bindings = {
      # Size management
      "Mod+R"                 = "switch-preset-column-width";
      "Mod+Shift+R"           = "switch-preset-column-width-back";
      "Mod+Ctrl+Shift+R"      = "switch-preset-window-height";

      "Mod+Minus"   = ''set-column-width "-10%"'';
      "Mod+Kp_Add"  = ''set-column-width "+10%"'';
      "Mod+Shift+Minus" = ''set-window-height "-10%"'';
      "Mod+Shift+Kp_Add" = ''set-window-height "+10%"'';

      # Layout management
      "Mod+T"       = "toggle-column-tabbed-display";
      "Mod+W"       = "toggle-window-floating";
      "Mod+Shift+W" = "switch-focus-between-floating-and-tiling";
      "Mod+Q repeat=false"  = "close-window";
      "Mod+C"       = "center-column";
      "Mod+Shift+C" = "center-window";
      "Mod+Ctrl+C"  = "center-visible-columns";
      "Mod+F"       = "maximize-column";
      "Mod+Shift+F" = "fullscreen-window";
      "Mod+M"       = "maximize-window-to-edges";
      "Mod+A"       = "toggle-window-rule-opacity";
      "Mod+Comma"        = "consume-window-into-column";
      "Mod+Shift+Comma"  = "expel-window-from-column";
      "Mod+Escape"  = "toggle-keyboard-shortcuts-inhibit";

       # Screenshots
      "Print"       = ''spawn-sh "${config.custom.programs.screenshot.exec.screen}"'';
      "Shift+Print" = ''spawn-sh "${config.custom.programs.screenshot.exec.region}"'';
      "Mod+Shift+S" = ''spawn-sh "${config.custom.programs.screenshot.exec.window}"'';

      # Screencasting
      "Mod+Shift+P"      = "set-dynamic-cast-window";
      "Mod+Ctrl+P"       = "set-dynamic-cast-monitor";
      "Mod+Ctrl+Shift+P" = "clear-dynamic-cast-target";

      # Shortcuts
      "Mod+Space"         = ''spawn "${lib.getExe config.programs.fuzzel.package}"'';
      "Mod+Ctrl+Space"    = lib.mkIf config.programs.wlr-which-key.enable ''spawn-sh "${lib.getExe config.programs.wlr-which-key.package}"'';
      "Mod+Return"        = ''spawn-sh "${terminal}"'';
      "Mod+Period"        = ''spawn "${lib.getExe emoji}"'';
      "Mod+Shift+E"       = ''spawn "${lib.getExe pkgs.nautilus}"'';
      "Mod+K"             = ''spawn-sh "${lib.getExe config.custom.programs.niri-keyboard-layout.package} next"'';
      "Mod+Shift+Q"       = ''spawn-sh "${config.custom.programs.session.exec.dmenu}"'';
      "Mod+L"             = ''spawn-sh "${config.custom.programs.session.exec.lock}"'';
      "Mod+I"             = lib.mkIf config.custom.programs.status-glance.enable ''spawn "${lib.getExe config.custom.programs.status-glance.package}"'';

      # Focus management
      "Mod+Tab repeat=false" = "toggle-overview";
      "Mod+Grave"       = "focus-workspace-previous";
      "Mod+End"         = "focus-column-last";
      "Mod+Left"        = "focus-column-left";
      "Mod+Down"        = "focus-window-or-workspace-down";
      "Mod+Up"          = "focus-window-or-workspace-up";
      "Mod+Right"       = "focus-column-right";
      "Mod+Home"        = "focus-column-first";

      # Moving things around
      "Mod+Ctrl+Left"  = "move-column-left";
      "Mod+Ctrl+Down"  = "move-window-down-or-to-workspace-down";
      "Mod+Ctrl+Up"    = "move-window-up-or-to-workspace-up";
      "Mod+Ctrl+Right" = "move-column-right";
      "Mod+Ctrl+Shift+Down" = "move-workspace-down";
      "Mod+Ctrl+Shift+Up"   = "move-workspace-up";
      "Mod+BracketLeft"   = "consume-or-expel-window-left";
      "Mod+BracketRight"  = "consume-or-expel-window-right";

      # Audio
      "XF86AudioRaiseVolume allow-when-locked=true"     = ''spawn-sh "${volume} sink-increase"'';
      "XF86AudioLowerVolume allow-when-locked=true"     = ''spawn-sh "${volume} sink-decrease"'';
      "XF86AudioMute        allow-when-locked=true"     = ''spawn-sh "${volume} sink-toggle-mute"'';
      "XF86AudioMicMute     allow-when-locked=true"     = ''spawn-sh "${volume} source-toggle-mute"'';
      "XF86AudioPrev        allow-when-locked=true"     = ''spawn-sh "${playerctl} previous"'';
      "XF86AudioNext        allow-when-locked=true"     = ''spawn-sh "${playerctl} next"'';
      "XF86AudioPlay        allow-when-locked=true"     = ''spawn-sh "${playerctl} play-pause"'';
      "XF86AudioPause       allow-when-locked=true"     = ''spawn-sh "${playerctl} play-pause"'';
      "Shift+XF86AudioRaiseVolume allow-when-locked=true" = ''spawn-sh "${volume} sink-move-next"'';
      "Shift+XF86AudioLowerVolume allow-when-locked=true" = ''spawn-sh "${volume} sink-move-prev"'';

      # Brightness
      "XF86MonBrightnessUp   allow-when-locked=true" = ''spawn-sh "${brightness} increase"'';
      "XF86MonBrightnessDown allow-when-locked=true" = ''spawn-sh "${brightness} decrease"'';
    };
  };

  wayland.windowManager.niri.settings = {
    prefer-no-csd = { };
    screenshot-path = "${screenshot.directory}/${screenshot.format}";
    hotkey-overlay.skip-at-startup = { };

    environment = {
      # Electron
      NIXOS_OZONE_WL = "1";
      ELECTRON_OZONE_PLATFORM_HINT = "auto";
    };

    input = {
      keyboard = {
        xkb = {
          layout = "us,pt";
          variant = "euro,";
          options = "caps:ctrl_modifier";
        };
        track-layout = "global"; # Keep layout consistent across all windows
      };

      focus-follows-mouse._props.max-scroll-amount = "10%";
    };

    layout = {
      gaps = 6;
      center-focused-column = "never";
      always-center-single-column = { };
      background-color = "transparent";

      preset-column-widths._children = [
        { proportion = 0.33333; }
        { proportion = 0.5; }
        { proportion = 0.66667; }
        { proportion = 1.0; }
      ];
      default-column-width.proportion = 0.66667;

      preset-window-heights._children = [
        { proportion = 0.33333; }
        { proportion = 0.5; }
        { proportion = 0.66667; }
        { proportion = 1.0; }
      ];

      focus-ring = {
        width = 2;
        active-gradient._props = { from = colors.base0D; to = colors.base0E; angle = 45; };
        inactive-color = colors.base04;
        urgent-color = colors.base08;
      };

      shadow.on = { };

      tab-indicator = {
        width = 4;
        hide-when-single-tab = { };
        length._props.total-proportion = 0.5;
        place-within-column = { };
        active-color = colors.base0A;
        inactive-color = colors.base04;
        urgent-color = colors.base08;
        position = "right";
        gaps-between-tabs = 4;
      };
    };

    # Keyboard-triggered actions use short fixed durations (easing).
    # Gesture-sensitive actions (touchpad swipes) use springs to respond to finger velocity.
    # Springs: higher stiffness = snappier. damping-ratio=1.0 = no oscillation (critically damped).
    animations = {
      window-open.duration-ms = 150;
      window-close.duration-ms = 150;
      window-resize.duration-ms = 150;
      window-movement.duration-ms = 150;
      workspace-switch.spring._props = { damping-ratio = 1.0; stiffness = 1200; epsilon = 0.0001; };
      horizontal-view-movement.spring._props = { damping-ratio = 1.0; stiffness = 1000; epsilon = 0.0001; };
      overview-open-close.spring._props = { damping-ratio = 1.0; stiffness = 1000; epsilon = 0.0001; };
    };

    cursor = {
      xcursor-theme = config.stylix.cursor.name;
      xcursor-size = config.stylix.cursor.size;
    };

    overview.workspace-shadow.off = { };

    gestures.hot-corners.off = { };
  };
}
