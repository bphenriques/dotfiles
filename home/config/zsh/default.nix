{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Basic Packages
    coreutils   # Consistency across different Operating Systems.
    gnugrep     # Consistency across different Operating Systems.
    zsh         # The Shell.
    vivid       # LS_COLORS generator because I refuse to use the syntax >.<
    unstable.direnv      # Automatically load .envrc or .env.
    unstable.nix-direnv  # Faster direnv for nix environments.

    # Zsh Plugins
    zsh-powerlevel10k               # Zsh theme.
    zsh-fast-syntax-highlighting    # Zsh syntax highlight.

    # Utilitary
    ripgrep     # Alternative to grep.
    fzf         # Fuzzy search.
    bat         # Better file preview.
    thefuck     # Amend previous command.
  ];

  # I manage my own zsh folder as I enjoy knowing what I put there.
  home.file.".zshenv".source = ./zshenv;
  xdg.configFile = {
    # Main files
    "zsh/.zprofile".source               = ./zprofile;
    "zsh/.zshrc".source                  = ./zshrc;
    "zsh/config.zsh".source              = ./config.zsh;
    "zsh/aliases.zsh".source             = ./aliases.zsh;
    "zsh/auto-completions.zsh".source    = ./auto-completions.zsh;
    "zsh/functions".source               = ./functions;

    # Setup Zsh Plugins
    "zsh/plugins.zsh".text               = ''
      (( ''${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"

      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      (( ''${+commands[direnv]} )) && emulate zsh -c "$(direnv hook zsh)"
      export DIRENV_LOG_FORMAT=

      # Load Plugins
      . ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      . ${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh
      . ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
      export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#3e4551"

      # Load Theme - https://github.com/romkatv/powerlevel10k
      . "$ZDOTDIR/powerlevel10k.theme.zsh"
    '';
    "direnv/direnvrc".text = "source ${pkgs.unstable.nix-direnv}/share/nix-direnv/direnvrc"; # Add nix-direnv extension.
    "zsh/powerlevel10k.theme.zsh".source = ./powerlevel10k.theme.zsh;
    "zsh/modules/thefuck.zsh".text = ''eval $(${pkgs.thefuck}/bin/thefuck --alias)'';
  };
}
