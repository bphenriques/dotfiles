{ pkgs, lib, self, ... }:
let
  fd = "${lib.getExe pkgs.fd} --hidden --exclude=.git";
  preview = "--preview '${lib.getExe self.packages.preview} {}'";
in
{
  programs.fzf = {
    enable = true;
    defaultCommand = "${fd} --type file";

    # fd instead of fzf's walker so .gitignore is respected.
    fileWidget = { command = fd; options = [ preview ]; };
    changeDirWidget = { command = "${fd} --type directory"; options = [ preview ]; };

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
