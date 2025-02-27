{ config, lib, pkgs, self, osConfig, ... }:


# Add eza which also avoids using ls color?
# TODO: kooha but doesnt work
# TODO: https://github.com/feschber/lan-mouse
# TODO: Enable console colorin in nixosg: config.stylix.targets.console.enable
# https://github.com/nyawox/nixboxes/blob/ecab4559da256b4f1198ca7d39d6e5b1d4442296/home/desktop/niri/general.nix
# Reference on how to create desktop itens next to executables: https://discourse.nixos.org/t/generate-and-install-a-desktop-file-along-with-an-executable/42744
# https://github.com/legendofmiracles/dotnix/blob/8dfa01af04d6391a1f5cb2c788bdecc1ee748ca9/hosts/pain/configuration.nix

# Alternative syntax: https://github.com/LilleAila/dotfiles/blob/main/home/home.nix#L16
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./mako.nix              # Notifications
    ./swayidle.nix          # Idle behaviour
    ./niri.nix              # Window Manager
    ./waybar                # Status bar
  ];

  custom.programs.swappy.enable = true;
  custom.programs.shortcuts.files.browser = "${lib.getExe' pkgs.foot "footclient"} --title=yazi-tui ${lib.getExe pkgs.yazi}";

  home.packages = [ pkgs.wdisplays ];
}