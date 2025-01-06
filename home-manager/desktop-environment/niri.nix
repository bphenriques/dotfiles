{ config, lib, pkgs, self, community, ... }:
# Logout: https://github.com/wuliuqii/nixos-config/blob/69606b2e0cccb6a135522fc5df188e4da0595e73/home/wm/wlogout.nix
# TODO: Alt F4 means keep closing active window until there is none. Then, show list of options.
# TODO: "Mod+Escape".action = spawn "wlogout";
#   # https://github.com/prasanthrangan/hyprdots?tab=readme-ov-file
# https://github.com/linyinfeng/dotfiles/blob/340f913ea7520a3c0034034d4fe870c05145bbcb/home-manager/profiles/niri/default.nix#L796
# ${lib.getExe pkgs.grim} -g \"$(${lib.getExe pkgs.slurp} -o -r -c '#ff0000ff')\" -t ppm - | ${lib.getExe pkgs.satty} --filename - --fullscreen --output-filename ~/Pictures/Screenshots/satty-$(date '+%Y%m%d-%H:%M:%S').png

# Battery? https://github.com/linuxmobile/kaku/blob/13eb9e8a19823cb2fa2aed29f7b1f49bea51c4a2/system/services/power.nix
# Seems to be more or less what I want? https://github.com/kiike/dotfiles/blob/ff788bae02ba6d15c73632d99654269d2b5fba49/hosts/balrog/default.nix
# Idle effect: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/swayidle.nix#L24
# Screencast? https://github.com/maximbaz/dotfiles/blob/98ff8b69370e86879faf57b29d07cfcb6aff4306/modules/linux/xdg.nix#L2
# https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix

# Window rules: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix#L143
# Env variables: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix#L185
# Funny login audio: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix#L201
# https://gitlab.com/scientiac/einstein.nixos/-/tree/main/home/niriwm?ref_type=heads
# https://gitlab.com/scientiac/einstein.nixos/-/blob/main/home/niriwm/niri.nix?ref_type=heads
# https://gitlab.com/usmcamp0811/dotfiles
# https://github.com/gopi487krishna/niri-waydots/tree/main/rofi

# TODO shortcut to lock the computer

let
  # nix repl
  # then :lf .
  # then inputs.nixpkgs.lib.strings.concatMapStringsSep " " (x: ''"${x}"'') inputs.nixpkgs.lib.strings.splitString " " "please run this command"
  # run-cmd = cmd: lib.strings.concatMapStringsSep " " (x: ''"${x}"'') lib.strings.splitString " " cmd;

  xwaylandDisplayId = "21";

  wallpapersPkg = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };

  env = {
    DISPLAY = ":${xwaylandDisplayId}";
    NIXOS_OZONE_WL = "1";                       # Electron?
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";  # Signal QT windows to remove their window decorations
  };

  on-startup = ''
    spawn-at-startup "${lib.getExe self.pkgs.swww-util}" "random" "${wallpapersPkg}/share/wallpapers"
    spawn-at-startup "${lib.getExe pkgs.xwayland-satellite}" ":${xwaylandDisplayId}"
    spawn-at-startup "${lib.getExe pkgs.waybar}"
    spawn-at-startup "${lib.getExe self.pkgs.niri-output-configuration}" "startup"
    spawn-at-startup  "${pkgs.wl-clipboard}/bin/wl-paste" "--type" "text" "--watch" "${lib.getExe pkgs.cliphist}" "store" "-max-items" "20"
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
  outputs = ''
    output "Samsung Display Corp. 0x4188 Unknown" {
      mode "2880x1800@120.001"
      scale 1.75
    }

    output "Dell Inc. DELL S2721DGF 4P11R83" {
      off
      mode "2560x1440@143.912"
      scale 1.0
      variable-refresh-rate on-demand=true
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
        width 3
        active-color "#7fc8ff"
        inactive-color "#505050"
      }

      border {
        off
      }
    }
  '';
in
{
  #services.gnome-keyring.enable = true;  # Redundant as done in nixos?

  xdg.configFile."niri/config.kdl".text = ''
    workspace "browsing"
    workspace "coding"
    workspace "gaming"

    window-rule {
      match at-startup=true app-id="com.mitchellh.ghostty"
      match at-startup=true app-id="jetbrains-idea-ce"
      open-on-workspace "coding"
    }

    window-rule {
      match at-startup=true app-id="firefox"
      open-on-workspace "browsing"
    }

    window-rule {
      match at-startup=true app-id="steam$"
      match at-startup=true app-id=r#"^steam_app_[0-9]+$"#
      open-on-workspace "gaming"
    }

    hotkey-overlay {
      skip-at-startup
    }

    environment {
      DISPLAY ":21"
      XDG_CURRENT_DESKTOP "niri"
      QT_WAYLAND_DISABLE_WINDOWDECORATION "1"
      QT_QPA_PLATFORM "wayland"
      NIXOS_OZONE_WL "1"
      MOZ_ENABLE_WAYLAND "1"
    }

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
      Mod+Shift+Slash { show-hotkey-overlay; }
      Mod+F { maximize-column; }
      Mod+Shift+F { fullscreen-window; }
      Mod+C { center-column; }
      Mod+W { spawn "pkill" "-SIGUSR1" "waybar"; }

      Print { screenshot; }
      Ctrl+Print { screenshot-screen; }
      Alt+Print { screenshot-window; }

      Mod+Shift+E { quit; }

      Mod+Period { spawn "${lib.getExe pkgs.bemoji}"; }
      Mod+Shift+Q { spawn "${lib.getExe self.pkgs.session-dmenu}"; }
      Mod+Shift+Tab { focus-workspace-previous; }
      Mod+Tab { spawn "${lib.getExe self.pkgs.niri-window-dmenu}"; }
      Mod+Shift+V { spawn "${lib.getExe self.pkgs.cliphist-dmenu}" "&&" "${lib.getExe self.pkgs.smart-paste}"; }

      // Suggested binds for running programs: terminal, app launcher, screen locker.
      Mod+Return { spawn "${lib.getExe pkgs.ghostty}"; }
      Mod+Space { spawn "${lib.getExe pkgs.fuzzel}"; }
      Super+Alt+L { spawn "swaylock"; }

      // Audio
      XF86AudioRaiseVolume allow-when-locked=true { spawn "${lib.getExe self.pkgs.osd-volume}" "increase"; }
      XF86AudioLowerVolume allow-when-locked=true { spawn "${lib.getExe self.pkgs.osd-volume}" "decrease"; }
      XF86AudioMute        allow-when-locked=true { spawn "${lib.getExe self.pkgs.osd-volume}" "toggle-mute"; }
      XF86AudioMicMute     allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }
      XF86AudioNext        allow-when-locked=true { spawn "playerctl" "next"; }
      XF86AudioPause       allow-when-locked=true { spawn "playerctl" "play-pause"; }
      XF86AudioPlay        allow-when-locked=true { spawn "playerctl" "play-pause"; }
      XF86AudioPrev        allow-when-locked=true { spawn "playerctl" "previous"; }

      // Brightness
      XF86MonBrightnessUp   allow-when-locked=true { spawn "${lib.getExe self.pkgs.osd-brightness}" "increase"; }
      XF86MonBrightnessDown allow-when-locked=true { spawn "${lib.getExe self.pkgs.osd-brightness}" "decrease"; }

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

      // Mod+Comma  { consume-window-into-column; }
      // Mod+Period { expel-window-from-column; }

      // There are also commands that consume or expel a single window to the side.
      Mod+BracketLeft  { consume-or-expel-window-left; }
      Mod+BracketRight { consume-or-expel-window-right; }

      Mod+R { switch-preset-column-width; }
      Mod+Shift+R { switch-preset-window-height; }
      Mod+Ctrl+R { reset-window-height; }

      Alt+Mod+Space       { switch-layout "next"; }

      // Powers off the monitors. To turn them back on, do any input like
      // moving the mouse or pressing any other key.
      Mod+Shift+P { power-off-monitors; }
    }
  '';
}
