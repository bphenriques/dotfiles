{ config, pkgs, lib, ... }:

{
  # MacOS specific settings
  imports = [
    ../macos/common.nix
    ../macos/personal.nix
  ];

  # Setup Home-manager
  environment.shells = [ pkgs.zsh ];
  home-manager.users.brunohenriques = {
    imports = [ ../home/common.nix ];
  };

  # Using Google DNS but I really should consider something else...
  networking = {
    knownNetworkServices = [ "Wi-Fi" ]; # List available ones with `networksetup -listallnetworkservices`
    dns = [ "8.8.8.8" "8.8.4.4" "2001:4860:4860::8888" "2001:4860:4860::8844" ];
  };
}
