{ config, pkgs, lib, ... }:
{
  xdg.enable = true;  # XDG Compliance to tidy up $HOME.
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
    unzip

    # Exploring
    gum           # TODO: Explore https://github.com/charmbracelet/gum
    helix

    # Search
    ripgrep     # Faster grep.
    jq          # Query JSON.
    fx          # Interactively navigate through JSON.
    yq          # Query YAML.
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
    (pkgs.writeShellScriptBin "docker" ''${lima}/bin/lima nerdctl $@'')
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    unrar
    xclip
  ];

  imports = [
    ./config/git
    ./config/scala
    ./config/zsh
    ./config/fzf
    ./config/wezterm
    ./config/neovim
  ];
}
