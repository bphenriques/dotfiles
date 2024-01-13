{ pkgs, lib, ... }:
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
    shfmt       # Format shell scripts
    nixfmt      # Format nix files

    # Monitoring
    procs       # Fancy `ps`.
    htop        # Fancy `top`.
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    lima    # Virtual Machine -  limactl start --set='.cpus = 4 | .memory = "10GiB"'
    (pkgs.writeShellScriptBin "docker" ''${lima}/bin/lima nerdctl $@'')
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    (nerdfonts.override { fonts = [ "Hack" ]; }) # User fonts. Darwin's fonts need to setup differently.
    unrar
    xclip
  ];

  home = {
    sessionVariables = {
      TERM    = "screen-256color";              # Ensure term is set with the right color

      # Set locale and UTF-8
      LANG    = "en_US.UTF-8";
      LC_ALL  = "en_US.UTF-8";

      # Default editors and settings
      EDITOR  = "hx";
      VISUAL  = "$EDITOR";
      PAGER   = "less -iMR";

      # Colors
      CLICOLOR  = 1;                                           # Enable ls colors in MacOS.
      LS_COLORS ="$(${pkgs.vivid}/bin/vivid generate snazzy)"; # LS_COLORS generator because I refuse to maintain one >.<

      WORKSPACE = "$HOME/workspace";            # Default directory for repositories
    };

    shellAliases = {
      # Default colorizatio
      diff = "diff --color=auto";
      grep = "grep --color=auto";
      egrep = "egrep --color=auto";
      fgrep = "fgrep --color=auto";
      ls = "ls --color=auto";

      # The usual aliases
      l = "ls -alh";
      ll = "ls -l";

      # Quality of life
      mkdir = "mkdir -pv";
      ".."  = "cd ..";
      "..."  = "cd ../..";
      ":q"   = "exit";
      tmpdir = "cd (mktemp -d)";

      # Text Processor
      e = "$EDITOR";

      # Utility
      whatsmyip = "curl ifconfig.me";
    };
  };

  fonts.fontconfig.enable = true;

  imports = [
    ./config/git
    ./config/helix
    ./config/firefox
    ./config/scala
    ./config/zsh
    ./config/zellij
    ./config/fish
    ./config/fzf
    ./config/wezterm
  ];
}
