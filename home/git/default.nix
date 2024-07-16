{ config, lib, pkgs, ... }:

# TODO: https://github.com/malob/nixpkgs/blob/master/home/gh-aliases.nix#L2
# TODO: https://github.com/Misterio77/nix-config/blob/cdc35ca281891268c6e9772cca1e66fb39de04ab/home/misterio/features/cli/git.nix
{
  home.packages = with pkgs; [
    git                             # Same git across OS.
    gitAndTools.diff-so-fancy       # Better diffs.
    lazygit                         # Cross-platform GUI to interact with Git
    git-absorb                      # Trying https://github.com/tummychow/git-absorb
  ];
  
  xdg.configFile = {
    "git/config".source      = ./gitconfig;
    "git/ignore".source      = ./ignore;
    "git/gitmessage".source  = ./gitmessage;
  };

  #custom.impermanence.config.directories = [
  #  ".config/systemd" # git maintenance systemd timers
  #];
}
