{ pkgs, lib, config, ... }:
{
  # Install zsh on the system level but do not really use it as a user (other than the zshenv!).
  # Found some oddicities in zsh:
  # - Shell aliases by default: https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/zsh/zsh.nix#L134
  # - Compinit by default: https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/zsh/zsh.nix#L134
  # - command_not_found_handler is enabled by default: https://github.com/NixOS/nixpkgs/blob/c93147ce79fb839543715ac9b6fd5908f832974d/nixos/modules/programs/command-not-found/command-not-found.nix#L28
  # - Default options already being set: https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/zsh/zsh.nix#L112
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  home.modules.zsh.options = [ "NO_GLOBAL_RCS" ];   # Skip loading zshrc under /etc/. It contains some defaults that I do not want.

  environment.systemPackages = with pkgs; [
    jetbrains.idea-community
    filezilla

    # Raspberry Pi
    rpi-imager
  ];

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };
  user.extraGroups = ["docker"];
}

