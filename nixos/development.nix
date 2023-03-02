{ pkgs, lib, config, ... }:
{
  environment.systemPackages = with pkgs; [
    jetbrains.idea-community
    filezilla
  ];

  # Development - Probabilly need add the user to the  "docker" group
  # virtualisation.docker.enable = true;
}
