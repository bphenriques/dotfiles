{ config, pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  stylix.targets.foot.enable = true;
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


