{ pkgs, config, lib, ... }:
{
  programs.gamemode = {
    enable = true;
    enableRenice = true;  # Ensure niceness is lower to increased priority
  };

  # TODO: https://github.com/bigbabyboost/dotfiles/blob/hyprnix/system/programs/games.nix#L2
}


