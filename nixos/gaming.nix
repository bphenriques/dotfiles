{ pkgs, lib, config, ... }:
{
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true; # Enable OpenGL 32bit programs for Wine when using a 64bit system.
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };
  systemd.extraConfig = "DefaultLimitNOFILE=1048576"; # Proton Games - Ref: https://github.com/zfigura/wine/blob/esync/README.esync

  environment.systemPackages = with pkgs; [
    lutris
    kdialog # Required for lutris?
    heroic-unwrapped
  ];
}
