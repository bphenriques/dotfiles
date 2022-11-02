{ config, pkgs, lib, ... }:
{
  xdg.enable = true;              # XDG Compliance to unclutter $HOME folder.
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

    # Modules
    ../modules/zsh.nix
    ../modules/fzf-extra.nix
    ../modules/thefuck.nix
    ../modules/direnv-extra.nix
    ../modules/powerlevel10k.nix
  ];

  home.stateVersion = "22.11";
}
