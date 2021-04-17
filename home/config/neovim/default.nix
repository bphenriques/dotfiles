{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ neovim ]; # Not using `enable = true;` as I manage by my own config.
  
  xdg.configFile = {
    "nvim/init.vim".source = ./init.vim;
  };
}
