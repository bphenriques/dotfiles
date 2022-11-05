{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.programs.direnv.extra;
in
{
  options.programs.direnv.extra = {
    personalZshIntegration = mkOption {
      type = bool;
      default = true;
    };

    enablePowerlevel10kFastPrompt = mkEnableOption "enable-powerlevel10k-fast-prompt";
    disableLogging = mkEnableOption "direnv-disable-log";
  };

  config = {
    modules.powerlevel10k.fastPrompt = mkIf cfg.enablePowerlevel10kFastPrompt {
      beforeInit = ''(( ''${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"'';
      afterInit = ''(( ''${+commands[direnv]} )) && emulate zsh -c "$(direnv hook zsh)"'';
    };

    modules.zsh = mkIf cfg.personalZshIntegration (mkMerge [
      {
        initExtraAfterPlugins = mkIf (!cfg.enablePowerlevel10kFastPrompt)
          ''eval "$(${pkgs.direnv}/bin/direnv hook zsh)"'';
      }
      {
        initExtraAfterPlugins = mkIf (cfg.disableLogging) ''export DIRENV_LOG_FORMAT='';
      }
    ]);
  };
}
