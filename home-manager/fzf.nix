{ pkgs, lib, config, ... }:
{
  custom.programs.fzf-fd.enable = true;   # Fuzzy fd
  custom.programs.fzf-rg.enable = true;   # Fuzzy ripgrep
  programs.fzf = {
    enable = true;
    defaultCommand = "${lib.getExe pkgs.fd} --type file --hidden --exclude=.git";
    enableFishIntegration = config.programs.fish.enable;

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
