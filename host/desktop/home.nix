{ config, pkgs, lib, ... }:
{
  # Programs
  programs.firefox.enable = true;
  services.dropbox.enable = true;

  # Gpg
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # Fonts
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    xclip
    python3
    rofi
    (nerdfonts.override { fonts = [ "Hack" ]; })
  ];
}
