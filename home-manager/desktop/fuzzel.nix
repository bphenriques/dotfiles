{ config, pkgs, lib, self, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.fuzzel = {
    enable = true;
    settings.main = {
      lines = 10;
      horizontal-pad = 30;
      vertical-pad = 10;
      inner-pad = 10;
      show-actions = true;
    };
  };
  stylix.targets.fuzzel.enable = true;
}
