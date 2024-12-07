{ config, lib, pkgs, self, community, ... }:
# Check custom scripts: https://github.com/iynaix/dotfiles/blob/fa261818c04e6b1aa7d928a10abd66e2c31c0ed9/packages/dotfiles-rs/dotfiles/src/bin/hypr-pip.rs
# https://github.com/dileep-kishore/nixos-hyprland/blob/main/home/common/optional/desktops/hyprland/config.nix
# https://github.com/JaKooLit/Hyprland-Dots/blob/main/config/hypr/configs/Keybinds.conf

# Force Quit active: https://github.com/JaKooLit/Hyprland-Dots/blob/main/config/hypr/configs/Keybinds.conf

# Logout: https://github.com/wuliuqii/nixos-config/blob/69606b2e0cccb6a135522fc5df188e4da0595e73/home/wm/wlogout.nix

# Consider going back to waybar: https://github.com/nix-community/nur-combined/blob/4d8b064e3cff836ee8c17c48c592874b0209e167/repos/slaier/modules/waybar/mediaplayer.nix

# Sway idle: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/swayidle.nix

# Login
# Greetd: https://github.com/linuxmobile/kaku/blob/13eb9e8a19823cb2fa2aed29f7b1f49bea51c4a2/system/services/greetd.nix#L5
# https://github.com/linuxmobile/kaku/blob/13eb9e8a19823cb2fa2aed29f7b1f49bea51c4a2/system/services/gdm.nix
# https://github.com/SergioRibera/dotfiles/blob/8e03a755e4e03b26722e6971effa4161c3efd0b6/hosts/common/services.nix#L45

# Setting up auto start: https://github.com/kiike/dotfiles/blob/ff788bae02ba6d15c73632d99654269d2b5fba49/home/features/desktop/tiling/ags.nix

# Battery? https://github.com/linuxmobile/kaku/blob/13eb9e8a19823cb2fa2aed29f7b1f49bea51c4a2/system/services/power.nix

# Seems to be more or less what I want? https://github.com/kiike/dotfiles/blob/ff788bae02ba6d15c73632d99654269d2b5fba49/hosts/balrog/default.nix

# Idle effect: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/swayidle.nix#L24

# Screencast? https://github.com/maximbaz/dotfiles/blob/98ff8b69370e86879faf57b29d07cfcb6aff4306/modules/linux/xdg.nix#L2

# TODO: Alt F4 means keep closing active window until there is none. Then, show list of options.
let

  # nix repl
  # then :lf .
  # then inputs.nixpkgs.lib.strings.concatMapStringsSep " " (x: ''"${x}"'') inputs.nixpkgs.lib.strings.splitString " " "please run this command"
  # run-cmd = cmd: lib.strings.concatMapStringsSep " " (x: ''"${x}"'') lib.strings.splitString " " cmd;

  wallpapersPkg = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };

  env = {
    QT_QPA_PLATFORM = "wayland";
    DISPLAY = ":1";
    NIXOS_OZONE_WL = "1";
    XDG_CURRENT_DESKTOP = "niri";
    XDG_SESSION_TYPE = "wayland";
    MOZ_ENABLE_WAYLAND = "1";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    # GBM_BACKEND=nvidia-drm
    # XDG_BACKEND=wayland
  };

  set-terminal = ''
    ${pkgs.swww}/bin/swww img "${wallpapersPkg}/share/wallpapers/mountains.png"
  '';

  # If Share picker doesn’t use the system theme
  # dbus-update-activation-environment --systemd --all
      #systemctl --user import-environment QT_QPA_PLATFORMTHEME

  # Ideas
  # "Mod+Return".action = spawn "${config.profile.terminal}";
  # "Mod+E".action = spawn "nautilus";
  # "Mod+Escape".action = spawn "wlogout";
/*
   touchpad = {
            tap = true;
            dwt = true;
            accel-profile = "adaptive";
            accel-speed = 0.0;
            click-method = "clickfinger";
            natural-scroll = true;
            scroll-method = "two-finger";
          };
*/
# Window rules: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix#L143
# Env variables: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix#L185

# Funny login audio: https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix#L201
in
{
  home.packages = with pkgs; [
    # pamixer ?
  ];

  services.gnome-keyring.enable = true;

  xdg.configFile."niri/config.kdl".text = ''
    workspace "coding"
    workspace "browsing"
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

    spawn-at-startup "${lib.getExe pkgs.swww}" "img" "--transition-type" "none" "${wallpapersPkg}/share/wallpapers/mountains.png"
    spawn-at-startup "xwayland-satellite" ":21"
    spawn-at-startup "${lib.getExe pkgs.waybar}"

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

    output "eDP-1" {
        mode "2880x1800@120.001"
        scale 1.75
    }

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

        // Suggested binds for running programs: terminal, app launcher, screen locker.
        Mod+Return { spawn "${lib.getExe community.pkgs.ghostty}"; }
        Mod+Space { spawn "${lib.getExe pkgs.fuzzel}"; }
        Super+Alt+L { spawn "swaylock"; }

        // Audio
        XF86AudioRaiseVolume allow-when-locked=true { spawn "${lib.getExe self.pkgs.dunst-volume}" "increase"; }
        XF86AudioLowerVolume allow-when-locked=true { spawn "${lib.getExe self.pkgs.dunst-volume}" "decrease"; }
        XF86AudioMute        allow-when-locked=true { spawn "${lib.getExe self.pkgs.dunst-volume}" "toggle-mute"; }
        XF86AudioMicMute     allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }
        XF86AudioNext        allow-when-locked=true { spawn "playerctl" "next"; }
        XF86AudioPause       allow-when-locked=true { spawn "playerctl" "play-pause"; }
        XF86AudioPlay        allow-when-locked=true { spawn "playerctl" "play-pause"; }
        XF86AudioPrev        allow-when-locked=true { spawn "playerctl" "previous"; }

        // Brightness
        XF86MonBrightnessUp   allow-when-locked=true { spawn "${lib.getExe self.pkgs.dunst-brightness}" "increase"; }
        XF86MonBrightnessDown allow-when-locked=true { spawn "${lib.getExe self.pkgs.dunst-brightness}" "decrease"; }

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

        // Switches focus between the current and the previous workspace.
        Mod+Tab { focus-workspace-previous; }

        Mod+Comma  { consume-window-into-column; }
        Mod+Period { expel-window-from-column; }

        // There are also commands that consume or expel a single window to the side.
        Mod+BracketLeft  { consume-or-expel-window-left; }
        Mod+BracketRight { consume-or-expel-window-right; }

        Mod+R { switch-preset-column-width; }
        Mod+Shift+R { switch-preset-window-height; }
        Mod+Ctrl+R { reset-window-height; }

        // Actions to switch layouts.
        // Note: if you uncomment these, make sure you do NOT have
        // a matching layout switch hotkey configured in xkb options above.
        // Having both at once on the same hotkey will break the switching,
        // since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
        // Mod+Space       { switch-layout "next"; }
        // Mod+Shift+Space { switch-layout "prev"; }

        // Powers off the monitors. To turn them back on, do any input like
        // moving the mouse or pressing any other key.
        Mod+Shift+P { power-off-monitors; }
    }
  '';
}
