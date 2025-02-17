{ config, pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  stylix.targets.foot.enable = true;

  # TODO: review: https://github.com/bigbabyboost/dotfiles/blob/hyprnix/home/terminal/emulators/foot.nix
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      scrollback.lines = 10000;
      mouse.hide-when-typing = "yes";
      csd.hide-when-maximized = true;
    };
  };
}


