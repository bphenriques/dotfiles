{ config, pkgs, lib, ... }:
{
  # Programs
  programs.firefox.enable = true;
  services.dropbox.enable = true; # TODO: Change path but ensure that the folders

  # Gpg
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # Media
  modules.programs.discord.enable = true;

  # Fonts
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    xclip
    python3
    rofi
    (nerdfonts.override { fonts = [ "Hack" ]; })
  ];
}
