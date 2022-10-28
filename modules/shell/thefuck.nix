{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.modules.thefuck;
in
{
  options.modules.thefuck = {
    enable = mkEnableOption "thefuck";
    enableZshIntegration = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ thefuck ];

    modules.zsh = mkIf cfg.enableZshIntegration {
      initExtraAfterPlugins = ''eval "$(${pkgs.thefuck}/bin/thefuck --alias)"'';
    };
  };
}
