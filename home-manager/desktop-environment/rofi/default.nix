{ config, pkgs, self, lib, ... }:

# https://github.com/umutsevdi/rofi-applets/tree/master/applets
#   #https://codeberg.org/adamcstephens/dotfiles/src/branch/main/apps/rofi/default.nix
# CALC
  # rofi -show calc -modi calc -theme launcher.rasi -terse -no-show-match -no-sort
  #   rofi -modi calc -show calc -calc-command 'xdotool type --clearmodifiers "{result}"'

# rofi -modi calc -show calc -no-show-match -no-sort

# More applets: https://github.com/Zhaith-Izaliel/rofi-applets
# https://github.com/jluttine/rofi-power-menu/blob/master/rofi-power-menu

# https://github.com/71zenith/kiseki/blob/2b85b44338b369a4d22baae2e684fa783e64afc2/home/rofi.nix#L248

# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi-power-menu.sh
# https://github.com/adi1090x/rofi/tree/master/files
# https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix
# desktop items: https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix

# Nice generator: https://github.com/71zenith/kiseki/blob/2b85b44338b369a4d22baae2e684fa783e64afc2/home/rofi.nix#L248
# https://github.com/edmundmiller/dotfiles/blob/main/bin/rofi/powermenu
#


# System monitor with:
# - Time
# - Battery (if applicable) (and potentially set the power profile)
# - Volume
# - Network
# - Keyboard
let
  dmenuGen = {
    name,
    dmenuRun ? ''rofi -dmenu -fixed-num-lines 9 -theme "${./themes/horizontal-symbols.rasi}"'',
    options ? { } # { "var" = { "exec": "...", "text": ""; "icon": } }
  }: pkgs.writeShellApplication {
      inherit name;
      text = ''
        shutdown=""
        reboot=""
        lock=""
        suspend="󰤄"
        windows=""
        system_monitor="󰞱"

        chosen="$(echo -e "$lock\n$suspend\n$reboot\n$windows\n$shutdown" | ${dmenuRun})"
        case ''${chosen} in
          "$shutdown")        systemctl poweroff                ;;
          "$reboot")          systemctl reboot                  ;;
          "$lock")           niri msg action do-screen-transition --delay-ms 1000 && hyprlock ;;
          "$windows")         reboot-to-windows                 ;;
          "$suspend")         systemctl suspend                 ;;
          "$system_monitor")  footclient --title=btop-tui btop  ;;
        esac
      '';
    };
in
{
  stylix.targets.rofi.enable = true;
  custom.desktop-environment = {
    # Not promoting as I prefer fuzzel as it opens faster.
    # application-launcher = ''rofi -show drun -theme ${./themes/launcher.rasi}'';

    # Alternative:  BEMOJI_PICKER_CMD="fuzzel -d" bemoji
    emoji-picker = lib.getExe (pkgs.writeShellApplication {
      name = "emoji-picker";
      text = ''rofi -modi emoji -show emoji -emoji-format "{emoji}" -theme ${./themes/emoji.rasi}''; # FIXME: Hack around niri spawn arguments
    });
    window-switcher = lib.getExe self.pkgs.niri-window-dmenu; #''rofi -modi window -show window -theme ${./themes/window.rasi}'';
    session-menu = lib.getExe self.pkgs.session-dmenu;#(dmenuGen { name = "session-menu"; });
  };

  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    plugins = [
      pkgs.rofi-emoji-wayland
      (pkgs.rofi-calc.override { rofi-unwrapped = pkgs.rofi-wayland-unwrapped; })
    ];
    terminal = config.custom.desktop-environment.terminal;
  };

  xdg = {
    configFile = {
      "rofi/shared.rasi".source = ./shared.rasi;
    };
  };

  xdg.dataFile = {
    #"rofi/themes/calc"
  };
}

# -fixed-num-lines ?