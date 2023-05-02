{ pkgs, lib, config, ... }:
{
  environment.systemPackages = with pkgs; [
    jetbrains.idea-community
    filezilla
  ];

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };
}
