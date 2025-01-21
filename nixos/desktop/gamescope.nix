{ pkgs, config, lib, ... }:
{
  # Steam's micro-compositor for gaming.
  programs.gamescope = {
    enable = true;
    capSysNice = true;  # Ensure niceness is lower to increased priority
    args = [];          # TODO: Consider default options
  };
}


