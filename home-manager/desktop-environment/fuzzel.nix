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
  custom.desktop-environment.application-launcher = lib.getExe pkgs.fuzzel;
  stylix.targets.fuzzel.enable = true;
  custom.desktop-environment.emoji-picker = ''
    BEMOJI_ECHO_NEWLINE=false BEMOJI_PICKER_CMD="${lib.getExe pkgs.fuzzel} -d" ${lib.getExe pkgs.bemoji}
  '';
}
