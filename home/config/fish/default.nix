{ lib, pkgs, ... }:

# https://github.com/jorgebucaran/awsm.fish?tab=readme-ov-file
# https://github.com/PatrickF1/fzf.fish
# TODO: https://github.com/ajeetdsouza/zoxide
# TODO: Missing dotfiles script
# TODO: Missing proj no longer needed if I use zoxide
# TODO: https://github.com/IlanCosman/tide/wiki/Configuration
with lib;
{
  home.packages = with pkgs; [ fish ];

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.direnv = {
    enable                  = true; # Automatically load .envrc or .env.
    nix-direnv.enable       = true; # Faster direnv for nix environments.
    extra.disableLogging    = true; # Disable verbose messages when entering a directory.
  };

  # Set emacs keybinding
  #         (optionalString (pkgs.stdenv.system == "aarch64-darwin") ''eval "$(/opt/homebrew/bin/brew shellenv)"'')
  #         ''[ -s "$HOME"/.zshenv.local ] && source "$HOME"/.zshenv.local''
  #       initExtraAfterCompInit = ''[ -s "$HOME"/.zshrc.local ] && source "$HOME"/.zshrc.local'';
  programs.fish = {
    enable = true;
    # shellInit = lib.mkIf (pkgs.stdenv.system == "aarch64-darwin") ''eval "$(/opt/homebrew/bin/brew shellenv)"'';
    plugins = [
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
      { name = "tide"; src = pkgs.fishPlugins.tide.src; }
      { name = "fzf"; src = pkgs.fishPlugins.fzf.src; }
    ];
  };

  # Call fish_update_completions

  # Alias: abbr -a sch pacman -Ss
  # Shortcut: ALT + Right/Left arrow does not work
}
