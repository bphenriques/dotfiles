{ config, lib, pkgs, self, osConfig, ... }:


# Add eza which also avoids using ls color?
# kooha but doesnt work
# wl-screenrec: https://github.com/russelltg/wl-screenrec?tab=readme-ov-file
# https://github.com/MaxVerevkin/wlr-which-key
# https://gitlab.freedesktop.org/serebit/waycheck
# https://github.com/feschber/lan-mouse
# https://github.com/hyprwm/hyprpicker
# https://github.com/LilleAila/dotfiles/blob/main/home/modules/desktop/programs/misc/espanso/default.nix
# Recording? https://github.com/LilleAila/dotfiles/blob/main/home/modules/desktop/programs/misc/other.nix#L75C16-L75C26
# Theming Zathura: https://github.com/LilleAila/dotfiles/blob/main/home/modules/desktop/programs/misc/zathura.nix
# Idea: https://github.com/LilleAila/dotfiles/blob/main/home/modules/desktop/programs/misc/zathura.nix
# Translator:? https://github.com/LilleAila/dotfiles/blob/main/home/modules/school/default.nix#L22C18-L22C33
# Console colors: https://github.com/LilleAila/dotfiles/blob/main/nixosModules/utils/console.nix
# https://github.com/samumoil/homelab/blob/cc08d3f557ef03b4d54457e751d4287e97c5909a/nixos/swayhome/home/config/wlrwhichkey/default.nix#L49

# Alternative syntax: https://github.com/LilleAila/dotfiles/blob/main/home/home.nix#L16
let
  terminal = lib.getExe' config.programs.foot.package "footclient";
  system-monitor = ''${terminal} --title=btop-tui ${lib.getExe pkgs.btop}'';
  filebrowser = "${terminal} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";
in
{
  imports = [
    ./niri.nix              # Window Manager
    ./waybar                # Status bar
    ./mako.nix              # Notification Daemon
    ./fuzzel.nix            # Application Launcher
    ./rofi.nix              # Alternative customizable menu
    ./swayidle.nix          # Locks/suspends the computer when idle
    ./hyprlock.nix          # Lock screend
    ./osd.nix               # On Screen Display
    ./swappy.nix            # Edit screenshots
    ./swww.nix              # Backend to manage wallpapers
    ./wlr-which-key.nix     # Alternative to menus
  ];

  custom.desktop-environment.apps = {
    core = { inherit terminal;
      file-browser = "${terminal} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";
    };
    tools = { inherit system-monitor; };
  };
}