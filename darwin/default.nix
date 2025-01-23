{ pkgs, ... }:
{
  imports = [
    ./brew.nix
    ./hardware.nix
    ./preferences.nix
  ];

  services.nix-daemon.enable = true;

  home-manager.useGlobalPkgs   = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages = true;   # Install packages defined in home-manager.

  environment.systemPackages = [ pkgs.fish ]; # Install the shell
  environment.shells = [ pkgs.fish ];         # Register the shell that was installed (2 step process)

  system.includeUninstaller = false; # use 'nix run github:LnL7/nix-darwin#darwin-uninstaller'

  # Fonts (system-wide)
  fonts.packages = with pkgs; [
    pkgs.nerd-fonts.hack
    pkgs.nerd-fonts.jetbrains-mono
  ];
}
