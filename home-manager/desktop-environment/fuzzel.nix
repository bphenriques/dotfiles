{ config, pkgs, lib, self, ... }:
{
  stylix.targets.fuzzel.enable = true;
  programs.fuzzel.enable = true;
}
