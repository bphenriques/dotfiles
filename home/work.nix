{ config, pkgs, lib, ... }:
{
  # Consider moving some of these packages to project's shell.nix
  home.packages = with pkgs; [
    terraform
    python39Full
  ];
}
