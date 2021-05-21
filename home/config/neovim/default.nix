{ config, lib, pkgs, ... }:

{
  # Using package provided by contrib@neovim/neovim Github repo
  home.packages = with pkgs; [ neovim-nightly ];
  
  xdg.configFile = {
    "nvim/init.vim".source = ./init.vim;
  };
}
