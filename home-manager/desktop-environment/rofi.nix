{ pkgs, lib, ... }:

# desktop items: https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix
{
  stylix.targets.rofi.enable = true;

  #   #https://codeberg.org/adamcstephens/dotfiles/src/branch/main/apps/rofi/default.nix
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
  };
}

# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi.nix
# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi-wifi-menu.sh
# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi-power-menu.sh
# https://github.com/adi1090x/rofi/tree/master/files
# https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix
# https://github.com/edmundmiller/dotfiles/blob/main/config/rofi/bin/rofi-browsermenu
