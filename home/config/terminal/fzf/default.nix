{ lib, pkgs, ... }:

with lib;
{
  home.packages = with pkgs; [ preview frg ];
  programs.fzf = {
    enable = true;
    defaultCommand = "${pkgs.fd}/bin/fd --type file --hidden";
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
