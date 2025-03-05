{ config, lib, programs, pkgs, self, osConfig, ... }:
let
  inherit (config.custom.programs) swappy;

  volume                = lib.getExe self.pkgs.volume-osd;
  brightness            = lib.getExe self.pkgs.brightness-osd;
  systemctl             = lib.getExe' pkgs.systemd "systemctl";
  pidof                 = lib.getExe' pkgs.procps "pidof";
  niri                  = lib.getExe pkgs.niri;
  hyprlock              = lib.getExe pkgs.hyprlock;
  terminal              = lib.getExe' pkgs.foot "footclient";
  playerctl             = lib.getExe pkgs.playerctl;
  application-launcher  = lib.getExe pkgs.fuzzel;
  dmenu                 = "${lib.getExe pkgs.fuzzel} -d";
  window-switcher       = lib.getExe self.pkgs.niri-window-dmenu;
  files-browser         = "${terminal} --title=yazi-tui ${lib.getExe pkgs.yazi}";
  wlr-which-key         = lib.getExe pkgs.wlr-which-key;

  emoji = pkgs.writeShellApplication {
    name = "emoji-picker";
    runtimeInputs = [ pkgs.wtype ];
    text = ''BEMOJI_PICKER_CMD="${dmenu}" ${lib.getExe pkgs.bemoji} --noline --type --clip'';
  };

  session-dmenu = self.lib.builders.writeDmenuApplication pkgs {
    name = "session-dmenu";
    entries = [
      { label = "    Lock";                exec = ''${pidof} hyprlock || ${niri} msg action do-screen-transition --delay-ms 750 && ${hyprlock}''; }
      { label = "󰤄    Suspend";             exec = "${systemctl} suspend"; }
      { label = "    Shutdown";            exec = "${systemctl} poweroff"; }
      { label = "    Reboot";              exec = "${systemctl} reboot"; }
      { label = "    Reboot to EFI setup"; exec = "${systemctl} reboot --firmware-setup"; }
    ] ++ lib.optionals (osConfig.custom.boot.grub.windows.efiDevice != "") [
      { label = "    Reboot to Windows";   exec = osConfig.custom.boot.grub.windows.rebootPackage; }
    ];
  };

  # Good enough
  toNiriSpawn = command: lib.strings.concatMapStringsSep
    " "
    (x: ''"${x}"'')
    (lib.strings.splitString " " command);

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
in
lib.mkIf pkgs.stdenv.isLinux {
  custom.programs.niri = {
    enable = true;
    screenshotPath = "${swappy.directory}/${swappy.format}";

    environment = {
      DISPLAY = ":${toString config.custom.services.xwayland-satellite.displayId}";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      QT_QPA_PLATFORM = "wayland";
      NIXOS_OZONE_WL = "1";
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
      "Mod+Q"       = "close-window";
      "Mod+C"       = "center-column";
      "Mod+Shift+C" = "center-window";
      "Mod+F"       = "maximize-column";
      "Mod+Shift+F" = "fullscreen-window";
      "Mod+Comma"   = "consume-window-into-column";
      "Mod+Shift+Comma" = "expel-window-from-column";

      # Screenshots
      "Print"       = "screenshot-screen";
      "Ctrl+Print"  = "screenshot";

      # Shortcuts
      "Mod+Return"      = ''spawn "${terminal}"'';
      "Mod+Space"       = ''spawn "${application-launcher}"'';
      "Mod+Period"      = ''spawn "${lib.getExe emoji}"'';
      "Mod+Shift+Space" = ''spawn ${toNiriSpawn files-browser}'';
      "Mod+K"           = ''spawn "${lib.getExe self.pkgs.niri-keyboard-layout}" "next"'';
      "Mod+Shift+Q"     = ''spawn "${lib.getExe session-dmenu}"'';
      "Mod+W"           = ''spawn "pkill" "-SIGUSR1" "waybar"'';
      "Mod+G"           = lib.mkIf (config.custom.programs.wlr-which-key.enable) ''spawn "${wlr-which-key}" "global"'';

      # Focus management
      "Mod+Left"        = "focus-column-left";
      "Mod+Down"        = "focus-window-or-workspace-down";
      "Mod+Shift+Down"  = "focus-workspace-down";
      "Mod+Up"          = "focus-window-or-workspace-up";
      "Mod+Shift+Up"    = "focus-workspace-up";
      "Mod+Right"       = "focus-column-right";
      "Mod+Tab"         = ''spawn "${window-switcher}"'';
      "Mod+Home"        = "focus-column-first";
      "Mod+End"         = "focus-column-last";
      "Alt+Tab"         = "focus-window-previous";
      "Mod+O"           = "toggle-window-rule-opacity";
      "Mod+1" = "focus-workspace 1";
      "Mod+2" = "focus-workspace 2";
      "Mod+3" = "focus-workspace 3";
      "Mod+4" = "focus-workspace 4";
      "Mod+5" = "focus-workspace 5";
      "Mod+6" = "focus-workspace 6";
      "Mod+7" = "focus-workspace 7";
      "Mod+8" = "focus-workspace 8";
      "Mod+9" = "focus-workspace 9";

      # Moving things around
      "Mod+Ctrl+Left"  = "move-column-left";
      "Mod+Ctrl+Down"  = "move-window-down-or-to-workspace-down";
      "Mod+Ctrl+Up"    = "move-window-up-or-to-workspace-up";
      "Mod+Ctrl+Right" = "move-column-right";
      "Mod+BracketLeft"   = "consume-or-expel-window-left";
      "Mod+BracketRight"  = "consume-or-expel-window-right";

      "Mod+Ctrl+1" = "move-column-to-workspace 1";
      "Mod+Ctrl+2" = "move-column-to-workspace 2";
      "Mod+Ctrl+3" = "move-column-to-workspace 3";
      "Mod+Ctrl+4" = "move-column-to-workspace 4";
      "Mod+Ctrl+5" = "move-column-to-workspace 5";
      "Mod+Ctrl+6" = "move-column-to-workspace 6";
      "Mod+Ctrl+7" = "move-column-to-workspace 7";
      "Mod+Ctrl+8" = "move-column-to-workspace 8";
      "Mod+Ctrl+9" = "move-column-to-workspace 9";

      # Audio
      "XF86AudioRaiseVolume allow-when-locked=true" = ''spawn "${volume}" "increase"'';
      "XF86AudioLowerVolume allow-when-locked=true" = ''spawn "${volume}" "decrease"'';
      "XF86AudioMute        allow-when-locked=true" = ''spawn "${volume}" "toggle-mute"'';
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
        match is-active=false
        opacity 0.90
      }

      cursor {
        xcursor-theme "${config.stylix.cursor.name}"
        xcursor-size ${toString config.stylix.cursor.size}
      };
    '';
  };


  # Limitation on the yaml generation that breaks the file if the line gets long (the full exe + arg)
  custom.programs.wlr-which-key.menus.global = lib.mkIf config.custom.programs.wlr-which-key.enable {
    "r" = lib.mkIf config.custom.programs.screen-recorder.enable 
      (submenu "Screen Recorder" config.custom.programs.wlr-which-key.menus.screen-recorder);
    "s" = lib.mkIf config.custom.programs.screenshot.enable
      (submenu "Screenshot" config.custom.programs.wlr-which-key.menus.screenshot);
  };
}

