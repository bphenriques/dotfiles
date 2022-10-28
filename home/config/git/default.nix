{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    git                             # Same git across OS.
    gitAndTools.diff-so-fancy       # Better diffs.
  ];
  
  xdg.configFile = {
    "git/config".source      = ./gitconfig;
    "git/ignore".source      = ./ignore;
    "git/gitmessage".source  = ./gitmessage;
  };
}
