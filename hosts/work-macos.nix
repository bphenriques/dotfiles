{ config, pkgs, lib, ... }:

{
  # MacOS specific settings
  imports = [
    # Shared Settings
    ../macos/shared-settings.nix
    ../macos/shared-fonts.nix
    ../macos/homebrew/shared-packages.nix

    # Host specific settings
    ../macos/homebrew/work-packages.nix
  ];

  # Setup Home-manager
  home-manager.users."bruno.henriques" = {
    imports = [ ../home/shared-home.nix ];
  };

  homebrew.cleanup = "none";     # To support private Homebrew formulas.

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
