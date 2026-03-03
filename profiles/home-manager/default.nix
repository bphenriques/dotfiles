{ pkgs, lib, config, ... }:
{
  # XDG Compliance to tidy up $HOME.
  xdg.enable = true;
  home.preferXdgDirectories = true;

  home.packages = [
    pkgs.vim
    pkgs.tree
    pkgs.unzip
    pkgs.file
    pkgs.watch
    pkgs.htop
    pkgs.curl
    pkgs.less
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    # Consistency across different operating systems
    pkgs.coreutils
    pkgs.findutils
    pkgs.gnugrep
    pkgs.gnused
  ];

  home = {
    sessionVariables = {
      PAGER = "less -iMR";
      HISTFILE = "${config.xdg.stateHome}/bash/history";
      LESSHISTFILE = "${config.xdg.stateHome}/less/history";
    } // lib.optionalAttrs pkgs.config.allowUnfree {
      NIXPKGS_ALLOW_UNFREE = 1;
    };

    shellAliases = {
      # Quality of life
      e = "$EDITOR";
      mkdir = "mkdir -pv";
      ".." = "cd ..";
      "..." = "cd ../..";
      ":q" = "exit";
    } // lib.optionalAttrs pkgs.stdenv.isLinux {
      # Colorization (GNU tools support --color, BSD on Darwin does not)
      diff = "diff --color=auto";
      grep = "grep --color=auto";
      ls = "ls --color=auto";
    };
  };

  # Discard home-manager configuration manual.
  manual = {
    html.enable = false;
    manpages.enable = false;
    json.enable = false;
  };

  # Defensive hardening and to keep it explicit
  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
  ];
}
