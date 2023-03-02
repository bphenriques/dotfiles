{ config, pkgs, lib, ... }:
{
  xdg.enable = true;  # XDG Compliance to tidy up $HOME folder.
  home.packages = with pkgs; [
    # Consistent UNIX command line tools regardless of the OS
    coreutils
    findutils
    gnugrep
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
    vim         # editor

    # Dev
    shellcheck  # Linter for shell scripts.

    # Monitoring
    procs       # Fancy `ps`.
    htop        # Fancy `top`.
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
