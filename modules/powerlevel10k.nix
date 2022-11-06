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
    enableZshIntegration = mkEnableOption "powerlevel10k-zsh-integration";
    configuration = mkOption { type = path; };

    fastPrompt = {
      enable = mkEnableOption "fast-prompt";
      beforeInit =  mkOption {
        type = lines;
        default = "";
        description = ''
          Added before loading p10k-instant-prompt: commands with output or that require human intervention.
        '';
      };
      afterInit =  mkOption {
        type = lines;
        default = "";
        description = ''
          Added before loading p10k-instant-prompt: commands with output or that require human intervention.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ zsh-powerlevel10k ];

    xdg.configFile."powerlevel10k/${configFileName}".source = cfg.configuration;
    modules.zsh = mkIf cfg.enableZshIntegration {
      initExtraFirst = mkIf cfg.fastPrompt.enable ''
        # Powerlevel10k Fast Prompt - Commands that need to run before
        ${cfg.fastPrompt.beforeInit}

        # Powerlevel10k Fast Prompt - Load
        if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
          source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
        fi

        # Powerlevel10k Fast Prompt - Commands that need to run after
        ${cfg.fastPrompt.afterInit}
      '';

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
