{ config, ... }:
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

  programs.fish.enable = true;
  programs.zsh.enableGlobalCompInit = false;   # home-manager runs compinit; twice doubles startup.
  environment.shells = [ config.programs.fish.package ];  # Register the shell
}
