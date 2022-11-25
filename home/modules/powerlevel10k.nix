{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.modules.powerlevel10k;
  configFileName = "powerlevel10k.theme.zsh";
in
{
  options.modules.powerlevel10k = {
    enable = mkEnableOption "zsh-powerlevel10k";
    personalZshIntegration = mkEnableOption "powerlevel10k-zsh-integration";
    configuration = mkOption { type = path; };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ zsh-powerlevel10k ];

    xdg.configFile."powerlevel10k/${configFileName}".source = cfg.configuration;
    modules.zsh = mkIf cfg.personalZshIntegration {
      plugins = [
        {
          name = "zsh-powerlevel10k";
          src = pkgs.zsh-powerlevel10k;
          file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
          sourceExtra = ''. "${config.xdg.configHome}/powerlevel10k/${configFileName}"'';
        }
      ];
    };
  };
}
