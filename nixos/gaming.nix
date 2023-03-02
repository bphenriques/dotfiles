{ pkgs, lib, config, ... }:
{
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true; # Enable OpenGL 32bit programs for Wine.
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  environment.systemPackages = with pkgs; [
    heroic-unwrapped
  ];

}
