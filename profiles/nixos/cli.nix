{ pkgs, lib, config, ... }:
{
  environment.systemPackages = [
    pkgs.vim
    pkgs.tree
    pkgs.unzip
    pkgs.file
    pkgs.watch
    pkgs.htop
    pkgs.curl
    pkgs.less
  ];

  environment.shellAliases = {
    e = "$EDITOR";
    mkdir = "mkdir -pv";
    ".." = "cd ..";
    "..." = "cd ../..";
    ":q" = "exit";
    diff = "diff --color=auto";
    grep = "grep --color=auto";
    ls = "ls --color=auto";
  };

  environment.variables = {
    PAGER = "less -iMR";
  } // lib.optionalAttrs config.nixpkgs.config.allowUnfree {
    NIXPKGS_ALLOW_UNFREE = "1";
  };

  programs.command-not-found.enable = false;   # Needs channels, which this fleet does not use
}
