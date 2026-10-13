{ pkgs, config, self, ... }:
{
  imports = [
    ../settings.nix
    ./brew.nix
    ./hardware.nix
    ./preferences.nix
  ];

  programs.fish.enable = true;
  environment.shells = [ config.programs.fish.package ];  # Register the shell

  # Misc
  home-manager.useGlobalPkgs    = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages  = true;   # Install packages defined in home-manager.
}