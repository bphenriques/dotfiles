{ pkgs, lib, ... }:

{
  stylix.targets.rofi.enable = true;

  #   #https://codeberg.org/adamcstephens/dotfiles/src/branch/main/apps/rofi/default.nix
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    plugins = [
      pkgs.rofi-emoji-wayland
      (pkgs.rofi-calc.override { rofi-unwrapped = pkgs.rofi-wayland-unwrapped; })
    ];
  };
}

# rofi -modi calc -show calc -no-show-match -no-sort
# rofi -modi emoji -show emoji

# https://github.com/71zenith/kiseki/blob/2b85b44338b369a4d22baae2e684fa783e64afc2/home/rofi.nix#L248

# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi-power-menu.sh
# https://github.com/adi1090x/rofi/tree/master/files
# https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix
# desktop items: https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix


# System monitor with:
# - Time
# - Battery (if applicable) (and potentially set the power profile)
# - Volume
# - Network
# - Keyboard