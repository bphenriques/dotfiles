{ pkgs, lib, config, ... }:
{
  environment.systemPackages = with pkgs; [
    jetbrains.idea-community
    filezilla

    # Raspberry Pi
    rpi-imager
  ];

  virtualisation.docker.enable = true;
}

