{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ unstable.neovim ];
  
  xdg.configFile = {
    "nvim/init.vim".source = ./init.vim;
  };
}
