{ config, pkgs, lib, ... }:

{ 
  # MacOS specific settings
  imports = [
    ../macos/common.nix
    ../macos/work.nix
  ];

  # Setup Home-manager
  environment.shells = [ pkgs.zsh ];
  home-manager.users."brunohenriques" = {
    imports = [
      ../home/common.nix
      ../home/work.nix
    ];
  };
}
