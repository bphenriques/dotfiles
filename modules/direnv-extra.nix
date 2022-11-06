{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.programs.direnv.extra;
in
{
  options.programs.direnv.extra = {
    personalZshIntegration = mkEnableOption "direnv-extra-personal-zsh-integration";
    disableLogging = mkEnableOption "direnv-extra-disable-log";
  };

  config = {
    modules.zsh = mkIf cfg.personalZshIntegration {
      initExtraAfterPlugins = mkIf cfg.disableLogging ''export DIRENV_LOG_FORMAT='';
    };
  };
}
