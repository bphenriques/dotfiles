{ config, lib, pkgs, ... }:

# TODO: GH cli? https://github.com/malob/nixpkgs/blob/master/home/git.nix#L29
# https://github.com/malob/nixpkgs/blob/master/home/gh-aliases.nix#L2
{
  home.packages = with pkgs; [
    git                             # Same git across OS.
    gitAndTools.diff-so-fancy       # Better diffs.
    lazygit                         # Cross-platform GUI to interact with Git
  ];
  
  xdg.configFile = {
    "git/config".source      = ./gitconfig;
    "git/ignore".source      = ./ignore;
    "git/gitmessage".source  = ./gitmessage;
  };
}
