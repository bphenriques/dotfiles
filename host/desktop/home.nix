{ config, pkgs, lib, ... }:
{
  # programs
  programs.firefox.enable = true;

  # gpg
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # Fonts
  fonts.fontconfig.enable = true;
  home.packages = [
    (pkgs.nerdfonts.override { fonts = [ "Hack" ]; })
  ];
}
