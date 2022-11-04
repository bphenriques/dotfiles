{ config, lib, pkgs, ... }:

with lib;
let
  preview = pkgs.writeShellApplication {
    name = "preview";
    runtimeInputs = with pkgs; [
      bat       # Preview files.
      tree      # Preview directories.
      chafa     # Preview images.
      jq        # Preview JSON.
    ];
    text = fileContents ./scripts/preview.zsh;
  };
in
{
  home.packages = with pkgs; [ preview ];
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
      "--bind='alt-n:deselect-all'"
      "--bind='ctrl-f:jump'"
    ];

    # Ctrl+T
    fileWidgetCommand = "$FZF_DEFAULT_COMMAND";
    fileWidgetOptions = ["--preview 'preview {}'"];

    # Ctrl+R
    historyWidgetOptions = ["--preview 'echo {}' --preview-window down:3:hidden:wrap"];
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

    # TODO: migrate git completions as part of the TAB-TAB

    functions = [
      ./functions/frg.zsh
      ./functions/proj.zsh
      {
        name = "_fzf_comprun";
        text = ''
          local command=$1
          shift
          case "$command" in
            *)  fzf "$@" --preview ''\'preview {-1}''\' ;;
          esac
        '';
      }
    ];

    widgets = [
      {
        name = "_frg-find-file";
        text = fileContents ./widgets/_frg-find-file.zsh;
        keybinding = "^f";
      }
    ];

    initExtraBeforeCompInit = ''
      zstyle ':fzf-tab:complete:(cd|ls):*' fzf-preview 'tree -C $realpath | head -200'        # Preview folders.
      zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview 'git diff --color=always $word'
    '';
  };
}
