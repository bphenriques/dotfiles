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
    plugins = [
      {
        name = "zsh-fzf-tab";
        src = pkgs.zsh-fzf-tab;
        file = "share/fzf-tab/fzf-tab.plugin.zsh";
        sourceTiming = "after-compinit";
      }
    ];

    functions = [
      ./functions/frg.zsh
      ./functions/proj.zsh
      ./functions/_proj.zsh
    ];

    widgets = [
      {
        name = "frg-find-file";
        text = fileContents ./widgets/frg-find-file.zsh;
        keybinding = "^f";
      }
    ];

    completions = concatStringsSep "\n" [
      # Disable sorting of all completions.
      "zstyle ':completion:complete:*:options' sort false"

      # Enable zsh groups and set nicer shorcuts
      ''
      zstyle ':completion:*:descriptions' format '[%d]'
      zstyle ':fzf-tab:*' switch-group ',' '.'
      ''

      # Set colors of files and directories.
      "zstyle ':completion:*' list-colors \${(s.:.)LS_COLORS\}"

      # Set default preview (file or directories) but disable it if passing arguments or options.
      ''
      zstyle ':fzf-tab:complete:*:*' fzf-preview 'preview ''\${(Q)realpath''\}'
      zstyle ':fzf-tab:complete:*:options' fzf-preview
      zstyle ':fzf-tab:complete:*:argument-1' fzf-preview
      ''
    ];
  };
}
