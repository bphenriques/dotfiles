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
    dateutils

    # Interactive
    gum           # TODO: Explore https://github.com/charmbracelet/gum

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
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    lima    # Virtual Machine -  limactl start --set='.cpus = 4 | .memory = "10GiB"'
    docker  # Docker CLI
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    iproute2
  ];

  imports = [
    ./config/git
    ./config/scala
    ./config/emacs
    ./config/zsh
    ./config/fzf
    ./config/tmux
    ./config/wezterm
  ];

  home.stateVersion = "22.11";
}
