{ config, pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.fuzzel = {
    enable = true;
    settings.main = {
      lines = 10;
      horizontal-pad = 30;
      vertical-pad = 10;
      inner-pad = 10;
      show-actions = true;
      terminal = ''${lib.getExe config.my.programs.terminal.package} -e {cmd}'';
      icon-theme = config.stylix.icons."${config.stylix.polarity}";
      icons-enabled = true;
    };
  };
  stylix.targets.fuzzel.enable = true;
  programs.fuzzel.settings.colors.border = lib.mkForce "${config.lib.stylix.colors.base03}ff";

  my.programs.niri.layerRules = {
    base = [
      ''
        layer-rule {
          match namespace="^launcher$"
          shadow {
            on
          }

          geometry-corner-radius 10

          background-effect {
            blur true
          }
        }
      ''
    ];
  };
}
