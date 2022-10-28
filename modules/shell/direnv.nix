{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.modules.direnv;
in
{
  options.modules.direnv = {
    enable = mkEnableOption "direnv";

    nix-direnv = {
      enable = mkEnableOption "nix-direnv";
    };

    enableZshIntegration = mkOption {
      type = bool;
      default = false;
    };

    enablePowerlevel10kFastPrompt = mkOption {
      type = bool;
      default = false;
    };

    logFormat = mkOption {
      type = nullOr str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.direnv
    ] ++ optional cfg.nix-direnv.enable unstable.nix-direnv;

    modules.powerlevel10k.fastPrompt = mkIf (cfg.enableZshIntegration && cfg.enablePowerlevel10kFastPrompt) {
      beforeInit = ''(( ''${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"'';
      afterInit = ''(( ''${+commands[direnv]} )) && emulate zsh -c "$(direnv hook zsh)"'';
    };

    modules.zsh = mkIf (cfg.enableZshIntegration && !cfg.enablePowerlevel10kFastPrompt) {
      initExtraBeforePlugins = mkIf (!cfg.enablePowerlevel10kFastPrompt)
        ''eval "$(${pkgs.unstable.direnv}/bin/direnv hook zsh)"'';

      initExtraAfterPlugins = mkIf (cfg.logFormat != null)
        ''export DIRENV_LOG_FORMAT=${cfg.logFormat}'';
    };

    # Register nix-direnv extension.
    xdg.configFile."direnv/direnvrc".text = mkIf cfg.nix-direnv.enable
      "source ${pkgs.unstable.nix-direnv}/share/nix-direnv/direnvrc";
  };
}
