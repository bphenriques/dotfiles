{ config, pkgs, lib, self, ... }:
{
  programs.fuzzel = {
    enable = true;
    settings.main = {
      lines=10;
      horizontal-pad=30;
      vertical-pad=10;
      inner-pad=10;
      terminal = config.custom.desktop-environment.terminal;
    };
  };
  stylix.targets.fuzzel.enable = true;
}
