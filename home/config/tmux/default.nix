{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ tmux ]; # Not using `enable: true` as I manage by my own config.

  xdg.configFile = {
    "tmux/tmux.conf".source        = ./tmux.conf;
    "tmux/tmux.theme.conf".source  = ./tmux.theme.conf;
  };
}
