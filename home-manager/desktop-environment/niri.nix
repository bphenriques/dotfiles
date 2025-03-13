{ config, lib, programs, pkgs, self, osConfig, ... }:
let
  inherit (config.custom.programs) swappy;

  volume                = lib.getExe self.pkgs.volume-osd;
  brightness            = lib.getExe self.pkgs.brightness-osd;
  terminal              = lib.getExe' pkgs.foot "footclient";
  playerctl             = lib.getExe pkgs.playerctl;
  dmenu                 = "${lib.getExe pkgs.fuzzel} -d";
  files-browser         = "${terminal} --title=yazi-tui ${lib.getExe pkgs.yazi}";
  system-monitor        = "${terminal} --title=btop-tui ${lib.getExe config.programs.btop.package}";

  emoji = pkgs.writeShellApplication {
    name = "emoji-picker";
    runtimeInputs = [ pkgs.wtype ];
    text = ''BEMOJI_PICKER_CMD="${dmenu}" ${lib.getExe pkgs.bemoji} --noline --type --clip'';
  };

  # Good enough
  toNiriSpawn = command: lib.strings.concatMapStringsSep
    " "
    (x: ''"${x}"'')
    (lib.strings.splitString " " command);
in
{
  custom.programs.niri = {
    enable = true;
    screenshotPath = "${swappy.directory}/${swappy.format}";

    environment = {
      NIXOS_OZONE_WL = "1"; # Electron apps
    };

    input = {
      keyboard.xkb = {
        layout = "us,pt";
        variant = "euro,";
        options = "caps:ctrl_modifier";
      };
      touchpad = [ "tap" "natural-scroll" ];
      extraOptions = [ ''focus-follows-mouse max-scroll-amount="10%"'' ];
    };

    layout = ''
      gaps 6
      center-focused-column "never"
      always-center-single-column
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
      "Mod+Comma"   = "consume-window-into-column";
      "Mod+Shift+Comma" = "expel-window-from-column";

      # Screenshots
      "Print"       = ''screenshot-screen'';
      "Shift+Print" = ''screenshot'';
      "Mod+Print"   = ''spawn ${toNiriSpawn config.custom.programs.screenshot.exec.menu}'';
      "Mod+Shift+S" = ''spawn "screenshot" "region-edit"'';

      # Shortcuts
      "Mod+Space"         = ''spawn "${lib.getExe pkgs.fuzzel}"'';
      "Mod+Ctrl+Space"    = lib.mkIf (config.custom.programs.wlr-which-key.enable) ''spawn "${lib.getExe pkgs.wlr-which-key}" "global"'';
      "Mod+Return"        = ''spawn "${terminal}"'';
      "Mod+Period"        = ''spawn "${lib.getExe emoji}"'';
      "Mod+E"             = ''spawn ${toNiriSpawn files-browser}'';
      "Mod+K"             = ''spawn "${lib.getExe self.pkgs.niri-keyboard-layout}" "next"'';
      "Mod+Shift+Q"       = ''spawn ${toNiriSpawn config.custom.programs.session.exec.menu}'';
      "Ctrl+Shift+Escape" = ''spawn ${toNiriSpawn system-monitor}'';
      "Mod+L"             = ''spawn ${toNiriSpawn config.custom.programs.session.exec.lock}'';

      # Focus management
      "Mod+Tab"         = ''spawn "${lib.getExe self.pkgs.niri-window-dmenu}"'';
      "Alt+Tab"         = "focus-window-previous";
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
      "XF86AudioRaiseVolume allow-when-locked=true" = ''spawn "${volume}" "sink-increase"'';
      "XF86AudioLowerVolume allow-when-locked=true" = ''spawn "${volume}" "sink-decrease"'';
      "XF86AudioMute        allow-when-locked=true" = ''spawn "${volume}" "sink-toggle-mute"'';
      "XF86AudioMicMute     allow-when-locked=true" = ''spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"'';
      "XF86AudioPrev        allow-when-locked=true" = ''spawn "${playerctl}" "previous"'';
      "XF86AudioNext        allow-when-locked=true" = ''spawn "${playerctl}" "next"'';
      "XF86AudioPlay        allow-when-locked=true" = ''spawn "${playerctl}" "play-pause"'';
      "XF86AudioPause       allow-when-locked=true" = ''spawn "${playerctl}" "play-pause"'';

      # Brightness
      "XF86MonBrightnessUp   allow-when-locked=true" = ''spawn "${brightness}" "increase"'';
      "XF86MonBrightnessDown allow-when-locked=true" = ''spawn "${brightness}" "decrease"'';
    };

    extraConfig = ''
      window-rule {
        match app-id="Firefox"
        match app-id="Steam"
        open-maximized true
      }

      window-rule {
        match is-active=false
        opacity 0.90
      }

      cursor {
        xcursor-theme "${config.stylix.cursor.name}"
        xcursor-size ${toString config.stylix.cursor.size}
      };
    '';
  };
}