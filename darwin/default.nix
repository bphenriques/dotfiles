{ pkgs, config, ... }:
{
  imports = [
    ./brew.nix
    ./hardware.nix
    ./preferences.nix
  ];

  programs.fish.enable = true;
  environment.shells = [ config.programs.fish.package ];  # Register the shell

  # Fonts (system-wide)
  fonts.packages = with pkgs; [
    pkgs.nerd-fonts.hack
    pkgs.nerd-fonts.jetbrains-mono
  ];

  # Misc
  home-manager.useGlobalPkgs   = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages = true;   # Install packages defined in home-manager.
  system.includeUninstaller = false;     # use 'nix run github:LnL7/nix-darwin#darwin-uninstaller'
}
