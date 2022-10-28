{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.programs.fzf.extras;
in
{
  options.programs.fzf.extras = {
    personalZshIntegration = mkEnableOption "zsh-integration";
  };

  config.modules.zsh.initExtraAfterPlugins = mkIf cfg.personalZshIntegration
    ''
      . "${pkgs.fzf}/share/fzf/completion.zsh"
      . "${pkgs.fzf}/share/fzf/key-bindings.zsh"
    '';
}
