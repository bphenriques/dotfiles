{ config, pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.fuzzel = {
    enable = true;
    package = pkgs.fuzzel-git;
    settings.main = {
      lines = 10;
      horizontal-pad = 30;
      vertical-pad = 10;
      inner-pad = 10;
      show-actions = true;
      terminal = "${lib.getExe' config.programs.foot.package "footclient"}";
      icon-theme = config.stylix.iconTheme."${config.stylix.polarity}";
      icons-enabled = true;
    };
  };
  stylix.targets.fuzzel.enable = true;

  custom.programs.niri.layerRules.launchers = [ ''namespace="^launcher$"'' ];
}
