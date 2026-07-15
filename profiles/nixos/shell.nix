{ pkgs, lib, config, inputs, ... }:
{
  imports = [ inputs.nix-index-database.nixosModules.nix-index ];

  programs.fish.enable = true;

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

  # Replaces stock `command-not-found` with comma: `, {cmd}`
  programs.nix-index.enable = true;
  programs.nix-index-database.comma.enable = true;
  programs.command-not-found.enable = false;
}
