{ lib, pkgs, self, ... }:

with lib;
{
  home.packages = with self.pkgs; [ preview frg ];
  programs.fzf = {
    enable = true;
    defaultCommand = "${getExe pkgs.fd} --type file --hidden --exclude=.git";
    enableFishIntegration = true;

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
  };
}
