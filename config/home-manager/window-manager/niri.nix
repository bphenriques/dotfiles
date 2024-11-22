{ config, lib, pkgs, self, ... }:
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

# TODO: Alt F4 means keep closing active window until there is none. Then, show list of options.
let
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
    ${pkgs.swww}/bin/swww img "${wallpapersPkg}/share/wallpapers/mountains.png" --transition-type random
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
    swww
    # pamixer ?
  ];

  services.gnome-keyring.enable = true;
  systemd.user.services = {
    swww = {
      Unit = {
        Description = "Efficient animated wallpaper daemon for wayland";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        Type = "simple";
        ExecStart = ''${pkgs.swww}/bin/swww-daemon'';
        ExecStop = "${pkgs.swww}/bin/swww kill";
        Restart = "on-failure";
      };
    };
  };

  xdg.configFile."niri/config.kdl".text = ''
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

    spawn-at-startup "${pkgs.swww}/bin/swww" "img" "${wallpapersPkg}/share/wallpapers/mountains.png" "--transition-type" "random"
    spawn-at-startup "xwayland-satellite" ":21"

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

        // Focus windows and outputs automatically when moving the mouse into them.
        // Setting max-scroll-amount="0%" makes it work only on windows already fully on screen.
        // focus-follows-mouse max-scroll-amount="0%"
    }

    output "eDP-1" {
        mode "2880x1800@120.001"
        scale 1.75
    }

    layout {
        gaps 8

        // When to center a column when changing focus, options are:
        // - "never", default behavior, focusing an off-screen column will keep at the left
        //   or right edge of the screen.
        // - "always", the focused column will always be centered.
        // - "on-overflow", focusing a column will center it if it doesn't fit
        //   together with the previously focused column.
        center-focused-column "never"

        preset-column-widths {
            proportion 0.33333
            proportion 0.5
            proportion 0.66667
        }
        // preset-window-heights { }

        default-column-width { proportion 0.5; }
        // default-column-width {}

        // By default focus ring and border are rendered as a solid background rectangle
        // behind windows. That is, they will show up through semitransparent windows.
        // This is because windows using client-side decorations can have an arbitrary shape.
        //
        // If you don't like that, you should uncomment `prefer-no-csd` below.
        // Niri will draw focus ring and border *around* windows that agree to omit their
        // client-side decorations.
        //
        // Alternatively, you can override it with a window rule called
        // `draw-border-with-background`.

        // You can change how the focus ring looks.
        focus-ring {
            // How many logical pixels the ring extends out from the windows.
            width 2
            active-color "#7fc8ff"
            inactive-color "#505050"
        }

        border {
            off
        }
    }

    prefer-no-csd

    // You can change the path where screenshots are saved.
    // A ~ at the front will be expanded to the home directory.
    // The path is formatted with strftime(3) to give you the screenshot date and time.
    screenshot-path "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png"

    animations {
    }

    binds {
        // Basic
        Mod+Q { close-window; }
        Mod+Shift+Slash { show-hotkey-overlay; }
        Mod+F { maximize-column; }
        Mod+Shift+F { fullscreen-window; }
        Mod+C { center-column; }

        // Suggested binds for running programs: terminal, app launcher, screen locker.
        Mod+Return { spawn "konsole"; }
        Mod+T { spawn "konsole"; }
        Mod+Space { spawn "fuzzel"; }
        Super+Alt+L { spawn "swaylock"; }

        // Audio
        XF86AudioRaiseVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"; }
        XF86AudioLowerVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"; }
        XF86AudioMute        allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
        XF86AudioMicMute     allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }
        XF86AudioNext        allow-when-locked=true { spawn "playerctl" "next"; }
        XF86AudioPause       allow-when-locked=true { spawn "playerctl" "play-pause"; }
        XF86AudioPlay        allow-when-locked=true { spawn "playerctl" "play-pause"; }
        XF86AudioPrev        allow-when-locked=true { spawn "playerctl" "previous"; }

        // Brightness
        XF86MonBrightnessUp   allow-when-locked=true { spawn "brightnessctl" "s" "10%+"; }
        XF86MonBrightnessDown allow-when-locked=true { spawn "brightnessctl" "s" "10%-"; }


        Mod+Left  { focus-column-left; }
        Mod+Down  { focus-window-down; }
        Mod+Up    { focus-window-up; }
        Mod+Right { focus-column-right; }
        Mod+H     { focus-column-left; }
        Mod+J     { focus-window-down; }
        Mod+K     { focus-window-up; }
        Mod+L     { focus-column-right; }

        Mod+Ctrl+Left  { move-column-left; }
        Mod+Ctrl+Down  { move-window-down; }
        Mod+Ctrl+Up    { move-window-up; }
        Mod+Ctrl+Right { move-column-right; }
        Mod+Ctrl+H     { move-column-left; }
        Mod+Ctrl+J     { move-window-down; }
        Mod+Ctrl+K     { move-window-up; }
        Mod+Ctrl+L     { move-column-right; }

        // Alternative commands that move across workspaces when reaching
        // the first or last window in a column.
        // Mod+J     { focus-window-or-workspace-down; }
        // Mod+K     { focus-window-or-workspace-up; }
        // Mod+Ctrl+J     { move-window-down-or-to-workspace-down; }
        // Mod+Ctrl+K     { move-window-up-or-to-workspace-up; }

        Mod+Home { focus-column-first; }
        Mod+End  { focus-column-last; }
        Mod+Ctrl+Home { move-column-to-first; }
        Mod+Ctrl+End  { move-column-to-last; }

        Mod+Shift+Left  { focus-monitor-left; }
        Mod+Shift+Down  { focus-monitor-down; }
        Mod+Shift+Up    { focus-monitor-up; }
        Mod+Shift+Right { focus-monitor-right; }
        Mod+Shift+H     { focus-monitor-left; }
        Mod+Shift+J     { focus-monitor-down; }
        Mod+Shift+K     { focus-monitor-up; }
        Mod+Shift+L     { focus-monitor-right; }

        Mod+Shift+Ctrl+Left  { move-column-to-monitor-left; }
        Mod+Shift+Ctrl+Down  { move-column-to-monitor-down; }
        Mod+Shift+Ctrl+Up    { move-column-to-monitor-up; }
        Mod+Shift+Ctrl+Right { move-column-to-monitor-right; }
        Mod+Shift+Ctrl+H     { move-column-to-monitor-left; }
        Mod+Shift+Ctrl+J     { move-column-to-monitor-down; }
        Mod+Shift+Ctrl+K     { move-column-to-monitor-up; }
        Mod+Shift+Ctrl+L     { move-column-to-monitor-right; }

        Mod+Page_Down      { focus-workspace-down; }
        Mod+Page_Up        { focus-workspace-up; }
        Mod+U              { focus-workspace-down; }
        Mod+I              { focus-workspace-up; }
        Mod+Ctrl+Page_Down { move-column-to-workspace-down; }
        Mod+Ctrl+Page_Up   { move-column-to-workspace-up; }
        Mod+Ctrl+U         { move-column-to-workspace-down; }
        Mod+Ctrl+I         { move-column-to-workspace-up; }

        Mod+Shift+Page_Down { move-workspace-down; }
        Mod+Shift+Page_Up   { move-workspace-up; }
        Mod+Shift+U         { move-workspace-down; }
        Mod+Shift+I         { move-workspace-up; }

        Mod+WheelScrollDown      cooldown-ms=150 { focus-workspace-down; }
        Mod+WheelScrollUp        cooldown-ms=150 { focus-workspace-up; }
        Mod+Ctrl+WheelScrollDown cooldown-ms=150 { move-column-to-workspace-down; }
        Mod+Ctrl+WheelScrollUp   cooldown-ms=150 { move-column-to-workspace-up; }

        Mod+WheelScrollRight      { focus-column-right; }
        Mod+WheelScrollLeft       { focus-column-left; }
        Mod+Ctrl+WheelScrollRight { move-column-right; }
        Mod+Ctrl+WheelScrollLeft  { move-column-left; }

        // Usually scrolling up and down with Shift in applications results in
        // horizontal scrolling; these binds replicate that.
        Mod+Shift+WheelScrollDown      { focus-column-right; }
        Mod+Shift+WheelScrollUp        { focus-column-left; }
        Mod+Ctrl+Shift+WheelScrollDown { move-column-right; }
        Mod+Ctrl+Shift+WheelScrollUp   { move-column-left; }

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

        // Alternatively, there are commands to move just a single window:
        // Mod+Ctrl+1 { move-window-to-workspace 1; }

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

        // Finer width adjustments.
        // This command can also:
        // * set width in pixels: "1000"
        // * adjust width in pixels: "-5" or "+5"
        // * set width as a percentage of screen width: "25%"
        // * adjust width as a percentage of screen width: "-10%" or "+10%"
        // Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
        // set-column-width "100" will make the column occupy 200 physical screen pixels.
        Mod+Minus { set-column-width "-10%"; }
        Mod+Equal { set-column-width "+10%"; }

        // Finer height adjustments when in column with other windows.
        Mod+Shift+Minus { set-window-height "-10%"; }
        Mod+Shift+Equal { set-window-height "+10%"; }

        // Actions to switch layouts.
        // Note: if you uncomment these, make sure you do NOT have
        // a matching layout switch hotkey configured in xkb options above.
        // Having both at once on the same hotkey will break the switching,
        // since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
        // Mod+Space       { switch-layout "next"; }
        // Mod+Shift+Space { switch-layout "prev"; }

        Print { screenshot; }
        Ctrl+Print { screenshot-screen; }
        Alt+Print { screenshot-window; }

        // The quit action will show a confirmation dialog to avoid accidental exits.
        Mod+Shift+E { quit; }

        // Powers off the monitors. To turn them back on, do any input like
        // moving the mouse or pressing any other key.
        Mod+Shift+P { power-off-monitors; }
    }
  '';
}
