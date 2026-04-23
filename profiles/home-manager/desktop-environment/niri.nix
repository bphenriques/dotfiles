{ config, lib, pkgs, self, ... }:
let
  inherit (config.custom.programs) swappy;

  volume          = lib.getExe config.custom.programs.volume-osd.package;
  brightness      = lib.getExe config.custom.programs.brightness-osd.package;
  terminal        = lib.getExe pkgs.ghostty;
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
    screenshotPath = "${swappy.directory}/${swappy.format}";

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
        extraOptions = [ ''track-layout "global"'' ]; # Keep layout consistent across all windows
      };

      extraOptions = [
        ''focus-follows-mouse max-scroll-amount="10%"''
      ];
    };

    layout = ''
      gaps 6
      center-focused-column "never"
      always-center-single-column
      background-color "transparent"

      preset-column-widths {
        proportion 0.33333
        proportion 0.5
        proportion 0.66667
        proportion 1.0
      }
      default-column-width { proportion 0.66667; }

      preset-window-heights {
        proportion 0.33333
        proportion 0.5
        proportion 0.66667
        proportion 1.0
      }

      focus-ring {
        width 2
        active-gradient from="${config.lib.stylix.colors.withHashtag.base0D}" to="${config.lib.stylix.colors.withHashtag.base0E}" angle=45
        inactive-color "${config.lib.stylix.colors.withHashtag.base04}"
        urgent-color "${config.lib.stylix.colors.withHashtag.base08}"
      }

      shadow {
        on
      }

      tab-indicator {
        width 4
        hide-when-single-tab
        length total-proportion=0.5
        place-within-column
        active-color "${config.lib.stylix.colors.withHashtag.base0A}"
        inactive-color "${config.lib.stylix.colors.withHashtag.base04}"
        urgent-color "${config.lib.stylix.colors.withHashtag.base08}"
        position "right"
        gaps-between-tabs 4
      }
    '';

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
          "^(Extension)"
        ];

        pip = lib.map (title: ''title="${title}"'') [
          "^Picture in picture$"
          "^Discord Popout$"
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
          }
        ''
        ''
          window-rule {
            match is-active=false
            opacity 0.90
          }
        ''
      ];

      # Generic overrides — cross-cutting rules not tied to any app profile
      overrides = [
        ''
          window-rule {
            match is-urgent=true
            border {
              on
              active-color "${config.lib.stylix.colors.withHashtag.base08}"
              inactive-color "${config.lib.stylix.colors.withHashtag.base08}"
            }
          }
        ''
      ];
    };

    bindings = {
      # Size management
      "Mod+R"            = "switch-preset-column-width";
      "Mod+Ctrl+R"       = "switch-preset-column-width-back";
      "Mod+Shift+R"      = "switch-preset-window-height";

      "Mod+Minus"   = ''set-column-width "-10%"'';
      "Mod+Kp_Add"  = ''set-column-width "+10%"'';
      "Mod+Shift+Minus" = ''set-window-height "-10%"'';
      "Mod+Shift+Kp_Add" = ''set-window-height "+10%"'';

      # Layout management
      "Mod+T"       = "toggle-column-tabbed-display";
      "Mod+W"       = "toggle-window-floating";
      "Mod+Shift+W" = "switch-focus-between-floating-and-tiling";
      "Mod+Q"       = "close-window";
      "Mod+C"       = "center-column";
      "Mod+Shift+C" = "center-window";
      "Mod+Ctrl+C"  = "center-visible-columns";
      "Mod+F"       = "maximize-column";
      "Mod+Shift+F" = "fullscreen-window";
      "Mod+Ctrl+F"  = "maximize-window-to-edges";
      "Mod+A"       = "toggle-window-rule-opacity";
      "Mod+Comma"        = "consume-window-into-column";
      "Mod+Shift+Comma"  = "expel-window-from-column";
      "Mod+Escape"  = "toggle-keyboard-shortcuts-inhibit";

       # Screenshots
      "Print"       = ''screenshot-screen'';
      "Shift+Print" = ''screenshot'';
      "Mod+Print"   = ''spawn-sh "${config.custom.programs.screenshot.dmenu}"'';
      "Mod+Shift+S" = ''spawn-sh "${lib.getExe config.custom.programs.screenshot.package} region-edit"'';

      # Shortcuts
      "Mod+Space"         = ''spawn "${lib.getExe config.programs.fuzzel.package}"'';
      "Mod+Ctrl+Space"    = lib.mkIf (config.custom.programs.wlr-which-key.enable) ''spawn-sh "${lib.getExe config.custom.programs.wlr-which-key.package} global"'';
      "Mod+Return"        = ''spawn-sh "${terminal} +new-window"'';
      "Mod+Period"        = ''spawn "${lib.getExe emoji}"'';
      "Mod+Shift+E"       = ''spawn "${lib.getExe pkgs.nautilus}"'';
      "Mod+K"             = ''spawn-sh "${lib.getExe config.custom.programs.niri-keyboard-layout.package} next"'';
      "Mod+Shift+Q"       = ''spawn-sh "${config.custom.programs.session.exec.dmenu}"'';
      "Mod+L"             = ''spawn-sh "${config.custom.programs.session.exec.lock}"'';

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
      "XF86AudioRaiseVolume allow-when-locked=true" = ''spawn-sh "${volume} sink-increase"'';
      "XF86AudioLowerVolume allow-when-locked=true" = ''spawn-sh "${volume} sink-decrease"'';
      "XF86AudioMute        allow-when-locked=true" = ''spawn-sh "${volume} sink-toggle-mute"'';
      "XF86AudioMicMute     allow-when-locked=true" = ''spawn-sh "${volume} source-toggle-mute"'';
      "XF86AudioPrev        allow-when-locked=true" = ''spawn-sh "${playerctl} previous"'';
      "XF86AudioNext        allow-when-locked=true" = ''spawn-sh "${playerctl} next"'';
      "XF86AudioPlay        allow-when-locked=true" = ''spawn-sh "${playerctl} play-pause"'';
      "XF86AudioPause       allow-when-locked=true" = ''spawn-sh "${playerctl} play-pause"'';

      # Brightness
      "XF86MonBrightnessUp   allow-when-locked=true" = ''spawn-sh "${brightness} increase"'';
      "XF86MonBrightnessDown allow-when-locked=true" = ''spawn-sh "${brightness} decrease"'';
    };

   extraConfig = ''
      workspace "1" {}
      workspace "2" {}
      workspace "3" {}

      cursor {
        xcursor-theme "${config.stylix.cursor.name}"
        xcursor-size ${toString config.stylix.cursor.size}
      };

      overview {
        workspace-shadow {
          off
        }
      }

      gestures {
        hot-corners {
          off
        }
      }
    '';
  };
}
