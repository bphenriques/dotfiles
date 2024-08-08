{ config, lib, pkgs, ... }:

{
  programs.zellij = {
    enable = true;
    enableFishIntegration = false; # The order matters.
  };

  xdg.configFile = {
    "zellij/config.kdl".source = ./config.kdl;
    "zellij/layouts/custom.kdl".text = (import ./layouts/custom.nix { inherit pkgs lib; });
  };
}


