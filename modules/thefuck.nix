{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.modules.thefuck;
in
{
  options.modules.thefuck = {
    enable = mkEnableOption "thefuck";
    personalZshIntegration = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ thefuck ];

    modules.zsh = mkIf cfg.personalZshIntegration {
      initExtraAfterPlugins = ''eval "$(${pkgs.thefuck}/bin/thefuck --alias)"'';
    };
  };
}
