{ pkgs, config, lib, ... }:
{
  programs.gamemode = {
    enable = true;
    enableRenice = true;  # Ensure niceness is lower to increased priority
  };
}


