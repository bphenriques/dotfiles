{ config, pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  stylix.targets.foot.enable = true;
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main.shell = lib.getExe config.programs.fish.package; # Fish is not POSIX compliant, therefore is not a login shell.
      cursor = {
        style = "beam";
        beam-thickness = "2";
      };
      scrollback.lines = 10000;
      mouse.hide-when-typing = "yes";
      csd.hide-when-maximized = true;
    };
  };
}


