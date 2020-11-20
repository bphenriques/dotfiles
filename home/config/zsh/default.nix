{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    coreutils   # Consistency across different Operating Systems.
    zsh         # The Shell.
    vivid       # LS_COLORS generator because I refuse to use that syntax >.<
    
    # Utilitary
    ripgrep     # Alternative to grep.
    fzf         # Fuzzy search.
    bat         # Better file preview.
  ];

  # I manage my own zsh folder as I enjoy knowing what I put there.
  home.file.".zshenv".source = ./zshenv;
  xdg.configFile = {
    # Main files
    "zsh/.zprofile".source               = ./zprofile;
    "zsh/.zshrc".source                  = ./zshrc;
    "zsh/config.zsh".source              = ./config.zsh;
    "zsh/plugins.zsh".source             = ./plugins.zsh;
    "zsh/aliases.zsh".source             = ./aliases.zsh;
    "zsh/auto-completions.zsh".source    = ./auto-completions.zsh;
    "zsh/functions".source               = ./functions;
    
    # Theme
    "zsh/powerlevel10k.theme.zsh".source  = ./powerlevel10k.theme.zsh;
  }; 
}
