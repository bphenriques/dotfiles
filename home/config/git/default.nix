{ config, lib, pkgs, ... }:

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
