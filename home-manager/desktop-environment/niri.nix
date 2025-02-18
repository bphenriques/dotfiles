{ config, lib, programs, pkgs, self, community, ... }:
# https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix
let
  volume = lib.getExe self.pkgs.volume-osd;
  brightness = lib.getExe self.pkgs.brightness-osd;

  environment = ''
    environment {
      DISPLAY ":${toString config.custom.services.xwayland-satellite.displayId}"
      QT_WAYLAND_DISABLE_WINDOWDECORATION "1"
      QT_QPA_PLATFORM "wayland"
      NIXOS_OZONE_WL "1"
    }
  '';

  on-startup = ''
  '';

  input = ''
    input {
      keyboard {
        xkb {
          layout "us,pt";
          variant "euro,";
          options "caps:ctrl_modifier"
        }
      }

      touchpad {
        tap
        natural-scroll
      }

      mouse {
      }
    }
  '';

  # Niri requires at least one monitor.
  outputs = ''
    output "Samsung Display Corp. 0x4188 Unknown" {
      mode "2880x1800@120.001"
      scale 1.75
    }
  '';

  layout = ''
    layout {
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
    }
  '';

  window-rules = ''
    window-rule {
      match at-startup=true app-id="steam$"
      match at-startup=true app-id=r#"^steam_app_[0-9]+$"#
      open-on-workspace "gaming"
    }

    window-rule {
      match app-id="firefox$" title="^Picture-in-Picture$"
      match title="^Picture in picture$"
      match title="^Discord Popout$"

      open-floating true
      open-focused false
      default-column-width { fixed 480; }
      default-window-height { fixed 270; }
      default-floating-position x=32 y=32 relative-to="bottom-right"
    }

    window-rule {
      match title="Steam Settings"
      match title="^(pwvucontrol)"
      match title="^(Volume Control)"
      match title="^(dialog)"
      match title="^(file_progress)"
      match title="^(confirm)"
      match title="^(download)"
      match title="^(error)"
      match title="^(notification)"

      open-floating true
    }

    window-rule {
      match app-id="org.pulseaudio.pavucontrol"
      match title="nmtui-tui"

      default-column-width { fixed 800; }
      default-window-height { fixed 600; }

      open-floating true
    }

    window-rule {
      match title="btop-tui"

      default-column-width { fixed 1024; }
      default-window-height { fixed 768; }

      open-floating true
    }

    window-rule {
      match title="clipse-tui"

      open-floating true
      default-column-width { fixed 1000; }
      default-window-height { fixed 500; }
      default-floating-position x=32 y=32 relative-to="bottom-left"
    }
  '';

  # TODO: Does not work well with commands with `sh`
  spawnCmdToNiri = command: lib.strings.concatMapStringsSep
        " "
        (x: ''"${x}"'')
        (lib.strings.splitString " " command);
in
{
  wayland.systemd.target = "niri.service";

  xdg.configFile."niri/config.kdl".text = ''
    workspace "gaming"

    hotkey-overlay {
      skip-at-startup
    }

    ${window-rules}
    ${environment}
    ${on-startup}
    ${input}

    ${outputs}
    ${layout}

    prefer-no-csd

    screenshot-path "${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR}/%Y-%m-%d %H-%M-%S.png"

    animations {
    }

    binds {
      // Basic
      Mod+Q { close-window; }
      Mod+F { maximize-column; }
      Mod+Shift+F { fullscreen-window; }
      Mod+C { center-column; }
      Mod+W { spawn "pkill" "-SIGUSR1" "waybar"; }

      Print { screenshot; }
      Ctrl+Print { screenshot-screen; }
      Alt+Print { screenshot-window; }

      Mod+Period { spawn "anyrun"; }

      Mod+Shift+Q { spawn ${spawnCmdToNiri config.custom.desktop-environment.session-menu}; }
      // Mod+Shift+Tab { focus-window-previous; }
      Mod+Tab { spawn ${spawnCmdToNiri config.custom.desktop-environment.window-switcher}; }

      Mod+Return { spawn ${spawnCmdToNiri config.custom.desktop-environment.terminal}; }
      Mod+Space { spawn ${spawnCmdToNiri config.custom.desktop-environment.application-launcher}; }
      Mod+Shift+Space { spawn ${spawnCmdToNiri config.custom.desktop-environment.file-browser}; }
      Super+L { spawn ${spawnCmdToNiri config.custom.desktop-environment.screen-lock}; }

      // Audio
      XF86AudioRaiseVolume allow-when-locked=true { spawn "${volume}" "increase"; }
      XF86AudioLowerVolume allow-when-locked=true { spawn "${volume}" "decrease"; }
      XF86AudioMute        allow-when-locked=true { spawn "${volume}" "toggle-mute"; }
      XF86AudioMicMute     allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }
      XF86AudioNext        allow-when-locked=true { spawn "${lib.getExe pkgs.playerctl}" "next"; }
      XF86AudioPause       allow-when-locked=true { spawn "${lib.getExe pkgs.playerctl}" "play-pause"; }
      XF86AudioPlay        allow-when-locked=true { spawn "${lib.getExe pkgs.playerctl}" "play-pause"; }
      XF86AudioPrev        allow-when-locked=true { spawn "${lib.getExe pkgs.playerctl}" "previous"; }

      // Brightness
      XF86MonBrightnessUp   allow-when-locked=true { spawn "${brightness}" "increase"; }
      XF86MonBrightnessDown allow-when-locked=true { spawn "${brightness}" "decrease"; }

      Mod+Left  { focus-column-left; }
      Mod+Down  { focus-window-down; }
      Mod+Up    { focus-window-up; }
      Mod+Right { focus-column-right; }

      Mod+Ctrl+Left  { move-column-left; }
      Mod+Ctrl+Down  { move-window-down; }
      Mod+Ctrl+Up    { move-window-up; }
      Mod+Ctrl+Right { move-column-right; }

      Mod+Shift+Down      { focus-workspace-down; }
      Mod+Shift+Up        { focus-workspace-up; }

      Mod+1 { focus-workspace 1; }
      Mod+2 { focus-workspace 2; }
      Mod+3 { focus-workspace 3; }
      Mod+4 { focus-workspace 4; }
      Mod+5 { focus-workspace 5; }
      Mod+6 { focus-workspace 6; }
      Mod+7 { focus-workspace 7; }
      Mod+8 { focus-workspace 8; }
      Mod+9 { focus-workspace 9; }
      Mod+Ctrl+1 { move-column-to-workspace 1; }
      Mod+Ctrl+2 { move-column-to-workspace 2; }
      Mod+Ctrl+3 { move-column-to-workspace 3; }
      Mod+Ctrl+4 { move-column-to-workspace 4; }
      Mod+Ctrl+5 { move-column-to-workspace 5; }
      Mod+Ctrl+6 { move-column-to-workspace 6; }
      Mod+Ctrl+7 { move-column-to-workspace 7; }
      Mod+Ctrl+8 { move-column-to-workspace 8; }
      Mod+Ctrl+9 { move-column-to-workspace 9; }

      // There are also commands that consume or expel a single window to the side.
      Mod+BracketLeft  { consume-or-expel-window-left; }
      Mod+BracketRight { consume-or-expel-window-right; }

      Mod+R { switch-preset-column-width; }
      Mod+Shift+R { switch-preset-window-height; }
      Mod+Ctrl+R { reset-window-height; }
    }
  '';
}
