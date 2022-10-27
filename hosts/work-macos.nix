{ config, pkgs, lib, ... }:

{
  imports = [
    ../macos/common.nix
    ../macos/work.nix
  ];

  # Nix Darwin
  environment.shells = [ pkgs.zsh ];
  users.users."brunohenriques".home = "/Users/brunohenriques";

  # Home Manager
  home-manager.users."brunohenriques" = {
    imports = [
      ../home/common.nix
      ../home/work.nix
    ];
  };
}
