{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    coreutils   # Consistency across different OS.
    findutils   # Consistent find and xargs across different OS.
    watch       # Commands on loop.
    tree        # File navigation.
    ripgrep     # Alternative to grep. Use with `rg`.
    fzf         # Fuzzy search.
    jq          # Query JSON.
    bat         # Preview with code highlight.
    fd          # A better `find`.
    gnupg       # To manage GNUPG keys.
    pinentry    # To input keys.
    wget        # Download stuff.

    # Other tools that I use from time to time:
    httpie      # Alternative to curl. Use with `http`.
    ngrok       # Tunneling.
    parallel    # Useful to parallelize tasks.

    # Tools I am experimenting:
    procs       # Fancy `ps`.
    htop        # Fancy `top`.
    exa         # Fancy `ls`.
    tealdeer    # Faster `tldr`.
    yq          # Query YAML. Similar to JQ.
  ];

  imports = [
    ./config/git
    ./config/scala
    ./config/emacs
    ./config/kitty
    ./config/zsh
    ./config/fzf
    ./config/tmux    
  ];

  home.stateVersion = "22.05";
}
