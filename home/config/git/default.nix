{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    git                             # Should be already installed.
    gitAndTools.diff-so-fancy       # Better diffs.
    fzf                             # Fuzzy search.
    bat                             # Better file preview with code highlight.
  ];
  
  xdg.configFile = {
     "git/config".source                       = ./gitconfig;
     "git/ignore".source                       = ./ignore;
     "git/gitmessage".source                   = ./gitmessage;
     "zsh/modules/git_fzf_complete.zsh".source = ./fzf_complete.zsh;
  };
}
