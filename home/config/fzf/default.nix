{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    bat                             # Better file preview with code highlight.
  ];

  programs.fzf = {
    enable = true;

    # Ensure that my integration with zsh is enable but not the one from Home-Manager.
    enableZshIntegration = false;
    extras.personalZshIntegration = true;

    defaultCommand = "fd --type file --hidden";
    fileWidgetCommand = "$FZF_DEFAULT_COMMAND";
    defaultOptions = [
      "--height='80%'"
      "--preview-window='right:60%'"
      "--bind='ctrl-p:toggle-preview'"
      ''--bind "alt-a:select-all"''
      "--bind='ctrl-f:jump'"
      "--marker='* '"
      "--pointer='▶'"
    ];
  };

  modules.zsh.functions = [
    ./functions/_fzf_comprun.zsh
    ./functions/_fzf_complete_git.zsh
    ./functions/frg.zsh
    ./functions/proj.zsh
  ];
}
