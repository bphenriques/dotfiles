{ pkgs, lib, config, ... }:
{
  fonts.fontconfig.enable = true;
  home.packages = [
    pkgs.nerd-fonts.hack
    pkgs.nerd-fonts.jetbrains-mono
  ];
}
