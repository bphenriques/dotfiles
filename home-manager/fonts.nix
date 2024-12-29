{ pkgs, lib, config, ... }:
{
  fonts.fontconfig.enable = true;
  home.packages = [
    pkgs.nerd-fonts.hack
    pkgs.nerd-fonts.jetbrains-mono
    pkgs.nerd-fonts.fira-mono
    pkgs.nerd-fonts.fira-code
  ];
}
