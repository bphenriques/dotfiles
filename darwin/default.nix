{ pkgs, config, self, ... }:
{
  imports = [
    ./brew.nix
    ./hardware.nix
    ./preferences.nix
  ];

  programs.fish.enable = true;
  environment.shells = [ config.programs.fish.package ];  # Register the shell

  # Fonts (system-wide)
  fonts.packages = [
    self.theme.fonts.monospace.package
    self.theme.fonts.sansSeriff.package
  ];

  # Misc
  home-manager.useGlobalPkgs    = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages  = true;   # Install packages defined in home-manager.
  system.includeUninstaller     = false;  # use 'nix run github:LnL7/nix-darwin#darwin-uninstaller'
}
