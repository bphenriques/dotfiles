{ pkgs, ... }:
{
  imports = [
    ../settings.nix
    ./brew.nix
    ./hardware.nix
    ./home-manager.nix
    ./preferences.nix
  ];

  nix = {
    channel.enable = false;
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };

  # Register the shells
  programs.zsh.enable = true;
  programs.zsh.enableGlobalCompInit = false;   # home-manager runs compinit; twice doubles startup.
  environment.shells = [ pkgs.zsh ];
}
