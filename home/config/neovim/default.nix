{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ neovim ];
  
  xdg.configFile = {
    "nvim/init.vim".source = ./init.vim;
  };
}
