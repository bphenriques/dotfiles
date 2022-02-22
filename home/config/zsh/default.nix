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
    zinit                           # Zsh plugin manager.
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
      # Load Zsh Plugin Manager
      . "${pkgs.zinit}/share/zinit/zinit.zsh"       

      # Load Plugins
      zinit light "${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k"
      zinit light "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions"
      zinit light "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions"
      export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#3e4551"

      # Load Theme - https://github.com/romkatv/powerlevel10k
      . "$ZDOTDIR/powerlevel10k.theme.zsh"

      # Setup direnv with low verbosity
      export DIRENV_LOG_FORMAT=
      eval "$(direnv hook zsh)"
    '';
    "direnv/direnvrc".text = "source ${pkgs.unstable.nix-direnv}/share/nix-direnv/direnvrc"; # Add nix-direnv extension.
    "zsh/powerlevel10k.theme.zsh".source = ./powerlevel10k.theme.zsh;
    "zsh/modules/thefuck.zsh".text = ''eval $(${pkgs.thefuck}/bin/thefuck --alias)'';
  };
}
