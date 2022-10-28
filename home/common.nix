{ config, pkgs, lib, ... }:
{
  xdg.enable = true;              # XDG Compliance to unclutter $HOME folder.
  home.packages = with pkgs; [
    # Consistent UNIX command line tools
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

    # Modules
    ../modules/shell/zsh.nix
    ../modules/shell/fzf-extra.nix
    ../modules/shell/thefuck.nix
    ../modules/shell/direnv.nix
    ../modules/shell/powerlevel10k.nix
  ];

  home.stateVersion = "22.11";
}
