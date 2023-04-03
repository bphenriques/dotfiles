{ pkgs, lib, config, ... }:
{
  environment.systemPackages = with pkgs; [
    jetbrains.idea-community
    filezilla
  ];

  # Development
  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };
}
