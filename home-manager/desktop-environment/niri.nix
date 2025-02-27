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

  emoji = pkgs.writeShellApplication {
    name = "emoji-picker";
    runtimeInputs = [ pkgs.wtype ];
    text = ''BEMOJI_PICKER_CMD="${dmenu}" ${lib.getExe pkgs.bemoji} --noline --type --clip'';
  };

  session-dmenu = self.lib.builders.writeDmenuScript pkgs {
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
    };

    layout = ''
      gaps 6
      center-focused-column "on-overflow"
      preset-column-widths {
        proportion 0.33333
        proportion 0.5
        proportion 0.66667
        proportion 1.0
      }
      default-column-width { proportion 1.00; }
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
      "Mod+T"       = "toggle-column-tabbed-display";
      "Mod+Q"       = "close-window";
      "Mod+F"       = "maximize-column";
      "Mod+Shift+F" = "fullscreen-window";
      "Mod+C"       = "center-column";
      "Mod+W"       = ''spawn "pkill" "-SIGUSR1" "waybar"'';

      "Print"       = "screenshot-screen";
      "Ctrl+Print"  = "screenshot";

      "Mod+Shift+Q" = ''spawn "${lib.getExe session-dmenu}"'';
      "Mod+Tab"     = ''spawn "${window-switcher}"'';

      "Mod+Return"      = ''spawn "${terminal}"'';
      "Mod+Space"       = ''spawn "${application-launcher}"'';
      "Mod+Period"      = ''spawn "${lib.getExe emoji}"'';
      "Mod+Shift+Space" = ''spawn ${toNiriSpawn files-browser}'';

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

      "Mod+Left"  = "focus-column-left";
      "Mod+Down"  = "focus-window-down";
      "Mod+Up"    = "focus-window-up";
      "Mod+Right" = "focus-column-right";

      "Mod+Ctrl+Left"  = "move-column-left";
      "Mod+Ctrl+Down"  = "move-window-down";
      "Mod+Ctrl+Up"    = "move-window-up";
      "Mod+Ctrl+Right" = "move-column-right";

      "Mod+Shift+Down" = "focus-workspace-down";
      "Mod+Shift+Up"   = "focus-workspace-up";

      "Mod+1" = "focus-workspace 1";
      "Mod+2" = "focus-workspace 2";
      "Mod+3" = "focus-workspace 3";
      "Mod+4" = "focus-workspace 4";
      "Mod+5" = "focus-workspace 5";
      "Mod+6" = "focus-workspace 6";
      "Mod+7" = "focus-workspace 7";
      "Mod+8" = "focus-workspace 8";
      "Mod+9" = "focus-workspace 9";
      "Mod+Ctrl+1" = "move-column-to-workspace 1";
      "Mod+Ctrl+2" = "move-column-to-workspace 2";
      "Mod+Ctrl+3" = "move-column-to-workspace 3";
      "Mod+Ctrl+4" = "move-column-to-workspace 4";
      "Mod+Ctrl+5" = "move-column-to-workspace 5";
      "Mod+Ctrl+6" = "move-column-to-workspace 6";
      "Mod+Ctrl+7" = "move-column-to-workspace 7";
      "Mod+Ctrl+8" = "move-column-to-workspace 8";
      "Mod+Ctrl+9" = "move-column-to-workspace 9";

      "Mod+BracketLeft"   = "consume-or-expel-window-left";
      "Mod+BracketRight"  = "consume-or-expel-window-right";

      "Mod+R"       = "switch-preset-column-width";
      "Mod+Shift+R" = "switch-preset-window-height";
      "Mod+Ctrl+R"  = "reset-window-height";
    };
  };
}
