{ config, lib, pkgs, self, ... }:
let
  inherit (config.custom.programs) swappy;

  volume          = lib.getExe config.custom.programs.volume-osd.package;
  brightness      = lib.getExe config.custom.programs.brightness-osd.package;
  terminal        = lib.getExe' pkgs.foot "footclient";
  playerctl       = lib.getExe pkgs.playerctl;
  dmenu           = "${lib.getExe config.programs.fuzzel.package} -d";
  files-browser   = "${terminal} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";
  system-monitor  = "${terminal} --title=btop-tui ${lib.getExe config.programs.btop.package}";
  dunstctl        = lib.getExe' pkgs.dunst "dunstctl";

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
      keyboard.xkb = {
        layout = "us,pt";
        variant = "euro,";
        options = "caps:ctrl_modifier";
      };
      touchpad = [ "tap" "natural-scroll" "drag false" ];
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
        active-color "${config.lib.stylix.colors.withHashtag.base0D}"
        inactive-color "${config.lib.stylix.colors.withHashtag.base04}"
        urgent-color "${config.lib.stylix.colors.withHashtag.base08}"
      }

      border {
        off
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
      popups = lib.map (title: ''title="${title}"'') [
        "Steam Settings"
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
        "Volume Control"
        "nmtui-tui"
        "btop-tui"
        "yazi-tui"
      ];
    };

    # TODO: expand-column-to-available-width, center-visible-columns
    bindings = {
      # Size management
      "Mod+R"       = "switch-preset-column-width";
      "Mod+Shift+R" = "switch-preset-window-height";
      "Mod+Minus"   = ''set-column-width "-10%"'';
      "Mod+Kp_Add"  = ''set-column-width "+10%"'';
      "Mod+Shift+Minus" = ''set-window-height "-10%"'';
      "Mod+Shift+Kp_Add" = ''set-window-height "+10%"'';

      # Layout management
      "Mod+T"       = "toggle-column-tabbed-display";
      "Mod+W"       = "toggle-window-floating";
      "Mod+Q"       = "close-window";
      "Mod+C"       = "center-column";
      "Mod+Shift+C" = "center-window";
      "Mod+F"       = "maximize-column";
      "Mod+Shift+F" = "fullscreen-window";
      "Mod+Ctrl+Shift+F" = "maximize-window-to-edges";
      "Mod+Comma"        = "consume-window-into-column";
      "Mod+Shift+Comma"  = "expel-window-from-column";
      "Mod+O repeat=false" = "toggle-overview";

       # Screenshots
      "Print"       = ''screenshot-screen'';
      "Shift+Print" = ''screenshot'';
      "Mod+Print"   = ''spawn-sh "${config.custom.programs.screenshot.dmenu}"'';
      "Mod+Shift+S" = ''spawn-sh "${lib.getExe config.custom.programs.screenshot.package} region-edit"'';

      # Notifications
      "Mod+N"        = ''spawn-sh "${dunstctl} action"'';
      "Mod+Shift+N"  = ''spawn-sh "${dunstctl} context"'';
      "Mod+Ctrl+N"   = ''spawn-sh "${dunstctl} close"'';

      # Shortcuts
      "Mod+Space"         = ''spawn "${lib.getExe config.programs.fuzzel.package}"'';
      "Mod+Ctrl+Space"    = lib.mkIf (config.custom.programs.wlr-which-key.enable) ''spawn-sh "${lib.getExe config.custom.programs.wlr-which-key.package} global"'';
      "Mod+Return"        = ''spawn-sh "${terminal}"'';
      "Mod+Period"        = ''spawn "${lib.getExe emoji}"'';
      "Mod+E"             = ''spawn-sh "${files-browser}"'';
      "Mod+K"             = ''spawn-sh "${lib.getExe self.pkgs.niri-keyboard-layout} next"'';
      "Mod+Shift+Q"       = ''spawn-sh "${config.custom.programs.session.exec.dmenu}"'';
      "Ctrl+Shift+Escape" = ''spawn-sh "${system-monitor}"'';
      "Mod+L"             = ''spawn-sh "${config.custom.programs.session.exec.lock}"'';

      # Focus management
      "Mod+Tab"         = ''spawn "${lib.getExe self.pkgs.niri-window-dmenu}"'';
      #"Alt+Tab"         = "focus-window-previous";
      "Mod+End"         = "focus-column-last";
      "Mod+Left"        = "focus-column-left";
      "Mod+Down"        = "focus-window-or-workspace-down";
      "Mod+Shift+Down"  = "focus-workspace-down";
      "Mod+Up"          = "focus-window-or-workspace-up";
      "Mod+Shift+Up"    = "focus-workspace-up";
      "Mod+Right"       = "focus-column-right";
      "Mod+Home"        = "focus-column-first";

      # Moving things around
      "Mod+Ctrl+Left"  = "move-column-left";
      "Mod+Ctrl+Down"  = "move-window-down-or-to-workspace-down";
      "Mod+Ctrl+Up"    = "move-window-up-or-to-workspace-up";
      "Mod+Ctrl+Right" = "move-column-right";
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

    # TODO: Explore tiled state window rule from 25.05 release
    # TODO: Explore is_urgent to add border red   
    # TODO: Update to use another variant that is blurred: https://github.com/YaLTeR/niri/wiki/Overview#backdrop-customization
   extraConfig = ''
      window-rule {
        geometry-corner-radius 6
        clip-to-geometry true
      }

      window-rule {
        match app-id=r#"firefox$"#
        match app-id=r#"zen$"#
        match app-id="Steam"
        match app-id=r#"^discord$"#
        match app-id="jetbrains-idea-ce"

        open-maximized true
      }

      window-rule {
        match is-active=false
        opacity 0.90
      }

      window-rule {
        match app-id=r#"^Bitwarden$"#
        block-out-from "screen-capture"
      }

      layer-rule {
        match namespace="swww-daemon"
        place-within-backdrop true
      }
      
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
