{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    bat   # Preview files.
    tree  # Preview directories.
  ];

  programs.fzf = {
    enable = true;

    # Ensure that my integration with zsh is enable but not the one from Home-Manager.
    enableZshIntegration = false;
    extras.personalZshIntegration = true;

    defaultCommand = "fd --type file --hidden";
    defaultOptions = [
      "--height='80%'"
      "--marker='* '"
      "--pointer='▶'"
      "--preview-window='right:60%'"
      "--bind='ctrl-p:toggle-preview'"
      "--bind='alt-a:select-all'"
      "--bind='ctrl-f:jump'"
    ];

    # Ctrl+T
    fileWidgetCommand = "$FZF_DEFAULT_COMMAND";
    fileWidgetOptions =
      let
        previewFile = "([ -f {} ] && bat --style=numbers --color=always {})";
        previewFolder = "([ -d {} && tree -C {})";
      in ["--preview '(${previewFile} || ${previewFolder}) 2>/dev/null | head -200'"];

    # Ctrl+R
    historyWidgetOptions = ["--preview 'echo {}' --preview-window down:3:wrap"];
  };

  modules.zsh = {
    plugins = {
      list = [
        {
          name = "zsh-fzf-tab";
          src = pkgs.zsh-fzf-tab;
          file = "share/fzf-tab/fzf-tab.plugin.zsh";
        }
      ];
    };

    functions = [
      ./functions/_fzf_comprun.zsh
      ./functions/_fzf_complete_git.zsh
      ./functions/frg.zsh
      ./functions/proj.zsh
    ];

    initExtraBeforeCompInit = ''
      zstyle ':fzf-tab:complete:cd:*' fzf-preview 'tree -C $realpath | head -200'       # Preview folders.
      zstyle ':fzf-tab:complete:(unset|export):*' fzf-preview 'eval "echo \$$word"'     # Preview env variables.
    '';
  };
}
