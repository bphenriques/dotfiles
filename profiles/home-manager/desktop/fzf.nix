{ pkgs, lib, ... }:
{
  programs.fzf = {
    enable = true;
    defaultCommand = "${lib.getExe pkgs.fd} --type file --hidden --exclude=.git";

    defaultOptions = [
      "--height='80%'"
      "--layout=reverse"
      "--cycle"
      "--info=inline-right"
      "--wrap"
      "--marker='* '"
      "--pointer='▶'"
      "--preview-window='right:60%'"
      "--bind='ctrl-p:toggle-preview'"
      "--bind='alt-a:select-all'"
      "--bind='alt-n:deselect-all'"
      "--bind='ctrl-f:jump'"
    ];
  };
}
