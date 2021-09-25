{ config, pkgs, lib, ... }:
{
  # Basic packages
  home.packages = with pkgs; [
    # Common Tools:
    coreutils   # Consistency across different Operating Systems.
    watch       # Commands on loop.
    tree        # File navigation.
    ripgrep     # Alternative to grep. Use with `rg`.
    fzf         # Fuzzy search.
    jq          # Json Processor.
    bat         # Preview with code highlight.
    fd          # A better `find`.
    gnupg       # To manage GNUPG keys.
    pinentry    # To input keys.
    
    # Other tools that I use from time to time:
    httpie      # Alternative to curl. Use with `http`.
    ngrok       # Tunneling.
    hugo        # Blogging. TODO: Move to project instead.
    parallel    # Useful to parallelize tasks.

    # Tools I am experimenting:
    htop        # Fancy `top`.
    exa         # Fancy `ls`.
  ];

  imports = [
    ./config/git
    ./config/scala
    ./config/emacs
    ./config/alacritty
    ./config/zsh
    ./config/fzf
    ./config/tmux
    
    # Packages I am experimenting with.
    ./config/vscodium
  ];


  home.stateVersion = "21.05";
}
