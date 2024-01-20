{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ zellij ];
  xdg.configFile = {
    "zellij/config.kdl".source = ./config.kdl;
    "zellij/layouts/custom.kdl".text = (import ./layouts/custom.nix { inherit pkgs lib; });
  };
}


