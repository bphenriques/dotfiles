{ config, pkgs, lib, ... }:

{
  # MacOS specific settings
  imports = [
    # Shared Settings
    ../macos/shared-settings.nix
    ../macos/shared-fonts.nix
    ../macos/homebrew/shared-packages.nix

    # Host specific settings
    ../macos/dns-settings.nix
    ../macos/homebrew/personal-packages.nix
    ../macos/homebrew/intel-cpu-only-packages.nix
  ];

  # Setup Home-manager
  home-manager.users.brunohenriques = {
    imports = [ ../home/shared-home.nix ];
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
