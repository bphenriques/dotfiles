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
    fzf         # Fuzzy search.
    jq          # Query JSON.
    fd          # A better `find`.
    tealdeer    # Faster `tldr`.

    # Security
    gnupg       # To manage GNUPG keys.
    pinentry    # To input keys.

    # Web
    wget        # Download stuff.
    # ngrok       # Tunneling.

    # Text
    bat         # Preview with code highlight.

    # Coding
    gh          # Github Cli

    # Experimenting with:
    procs       # Fancy `ps`.
    htop        # Fancy `top`.
    exa         # Fancy `ls`.

    # FIXME: Broken packages due to https://github.com/NixOS/nixpkgs/issues/174457
    #httpie
    #yq          # Query YAML. Similar to JQ.
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
