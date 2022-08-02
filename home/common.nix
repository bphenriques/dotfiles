{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    # Consistent UNIX command line tools
    coreutils
    findutils
    watch
    tree
    parallel
    gnused

    # Search
    ripgrep     # Alternative to grep. Use with `rg`.
    fzf         # Fuzzy search.
    jq          # Query JSON.
    fd          # A better `find`.
    tealdeer    # Faster `tldr`.

    # Security
    gnupg       # To manage GNUPG keys.
    pinentry    # To input keys.

    # Web
    wget        # Download stuff.
    httpie      # Alternative to curl. Use with `http`.
    ngrok       # Tunneling.

    # Other
    bat         # Preview with code highlight.

    # Experimenting with:
    procs       # Fancy `ps`.
    htop        # Fancy `top`.
    exa         # Fancy `ls`.
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
