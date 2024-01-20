{ lib, pkgs, ... }:

with lib;
{
  home.packages = with pkgs; [ preview frg ];
  programs.fzf = {
    enable = true;

    # Ensure that my integration with zsh is enable but not the one from Home-Manager.
    enableZshIntegration = false;
    extra.personalZshIntegration = true;

    defaultCommand = "fd --type file --hidden";
    defaultOptions = [
      "--height='80%'"
      "--marker='* '"
      "--pointer='▶'"
      "--preview-window='right:60%'"
      "--bind='ctrl-p:toggle-preview'"
      "--bind='alt-a:select-all'"
      "--bind='alt-n:deselect-all'"
      "--bind='ctrl-f:jump'"
    ];

    # Ctrl+T
    fileWidgetCommand = "$FZF_DEFAULT_COMMAND";
    fileWidgetOptions = ["--preview '${pkgs.preview}/bin/preview {}'"];

    # Ctrl+R
    historyWidgetOptions = ["--preview 'echo {}' --preview-window down:3:hidden:wrap"];
  };

  modules.zsh = {
    widgets = [
      {
        name = "frg-find-file";
        text = ''
          LBUFFER+="$(${pkgs.frg}/bin/frg)"
          local ret=$?
          zle reset-prompt
          return $ret
        '';
        keybinding = "^f";
      }
    ];

    completions = concatStringsSep "\n" [
      # Disable sorting of all completions.
      "zstyle ':completion:complete:*:options' sort false"

      # Enable zsh groups and set nicer shorcuts
      ''
      zstyle ':completion:*:descriptions' format '[%d]'
      ''

      # Set colors of files and directories.
      "zstyle ':completion:*' list-colors \${(s.:.)LS_COLORS\}"
    ];
  };
}
