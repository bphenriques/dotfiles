{ config, ... }:
{
  programs.direnv = {
    enable                  = true; # Automatically load .envrc or .env.
    nix-direnv.enable       = true; # Faster direnv for nix environments.
    silent                  = true; # Disable verbose messages when entering a directory.
    config.whitelist.prefix = [ config.custom.dotfiles.directory ]; # Surpress prompt in my private dotfiles
  };

  programs.git.ignores = [ ".direnv/" ];
}
