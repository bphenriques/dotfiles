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
# https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix
# Reference on how to create desktop itens next to executables: https://discourse.nixos.org/t/generate-and-install-a-desktop-file-along-with-an-executable/42744

# Alternative syntax: https://github.com/LilleAila/dotfiles/blob/main/home/home.nix#L16
{
  imports = [
    ./niri.nix              # Window Manager
    ./waybar                # Status bar
  ];
}