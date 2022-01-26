{ config, pkgs, lib, ... }:

{
  # MacOS specific settings
  imports = [
    ../macos/common.nix
    ../macos/personal.nix
  ];

  # Using Google DNS but I really should consider something else...
  networking = {
    knownNetworkServices = [ "Wi-Fi" ]; # List available ones with `networksetup -listallnetworkservices`
    dns = [ "8.8.8.8" "8.8.4.4" "2001:4860:4860::8888" "2001:4860:4860::8844" ];
  };

  # Intel CPU - Nix Darwin sets it according to the environment but it doesnt work fine.
  homebrew.brewPrefix = "/usr/local/bin";
  homebrew.taps = [ "homebrew/cask" ];
  homebrew.casks = [
    "docker"                    # Containers.
    "intellij-idea-ce"          # IDE for JVM projects.
  ];

  # Setup Home-manager
  environment.shells = [ pkgs.zsh ];
  home-manager.users.brunohenriques = {
    imports = [ ../home/common.nix ];
  };
}
