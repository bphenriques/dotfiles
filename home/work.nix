{ config, pkgs, lib, ... }:
{
  # Consider moving some of these packages to project's shell.nix if team's okay with that.
  home.packages = with pkgs; [
    google-cloud-sdk
    awscli
    unstable.terraform
  ];
}
