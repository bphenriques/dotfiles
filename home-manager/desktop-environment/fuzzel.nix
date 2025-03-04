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
      terminal = "${lib.getExe' config.programs.foot.package "footclient"}";
      icon-theme = if config.stylix.polarity == "dark" then config.stylix.iconTheme.dark else config.styles.iconTheme.light;
      icons-enabled = true;
    };
  };
  stylix.targets.fuzzel.enable = true;

  custom.programs.niri.layerRules.launchers = [ ''namespace="^launcher$"'' ];
}
