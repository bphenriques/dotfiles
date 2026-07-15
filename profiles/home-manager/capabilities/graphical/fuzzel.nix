{ config, pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.fuzzel = {
    enable = true;
    settings.main = {
      lines = 10;
      horizontal-pad = 30;
      vertical-pad = 10;
      inner-pad = 10;
      show-actions = true;
      terminal = ''${lib.getExe config.custom.programs.terminal.package} -e {cmd}'';
      icon-theme = config.stylix.icons."${config.stylix.polarity}";
      icons-enabled = true;
    };
  };
  stylix.targets.fuzzel.enable = true;
  programs.fuzzel.settings.colors.background = lib.mkForce "${config.lib.stylix.colors.base00}EB"; # 92% opacity for blur (override stylix's FF alpha)

  custom.programs.niri.layerRules = {
    launchers = [ ''namespace="^launcher$"'' ];
    screencasting.block = [ ''namespace="^launcher$"'' ];
  };
}
