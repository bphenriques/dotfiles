{ pkgs, lib, config, ... }:
{
  # TODO: Interesting for Android dev: https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/home/direnv/lib/android.sh
  programs.direnv = {
    enable                  = true; # Automatically load .envrc or .env.
    nix-direnv.enable       = true; # Faster direnv for nix environments.
    silent                  = true; # Disable verbose messages when entering a directory.
    config.whitelist.prefix = [ config.custom.dotfiles.directory ]; # Surpress prompt in my private dotfiles
  };
}
