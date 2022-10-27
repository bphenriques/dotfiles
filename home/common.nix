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
    ripgrep     # Faster grep.
    jq          # Query JSON.
    fd          # A better `find`.
    tealdeer    # Faster `tldr`.

    # Security
    gnupg       # To manage GNUPG keys.
    pinentry    # To input keys.

    # Web
    wget        # Download stuff.

    # Text
    bat         # Preview with code highlight.
    nb          # Notebook CLI tool.

    # Coding
    gh          # Github Cli

    # Experimenting with:
    procs       # Fancy `ps`.
    htop        # Fancy `top`.
    exa         # Fancy `ls`.
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

  home.stateVersion = "22.11";
}
