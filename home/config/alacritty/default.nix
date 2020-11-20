{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ alacritty ]; # Not using `enable: true` as I manage by my own config.
  
  xdg.configFile = {
    "alacritty/alacritty.yml".source = ./alacritty.yml;
  };
}

