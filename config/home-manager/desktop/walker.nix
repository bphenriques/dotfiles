{ config, pkgs, lib, community, ... }:
{
  # Alternative that does not require a external flake: https://codeberg.org/dnkl/fuzzel
  home.packages = [ community.pkgs.walker pkgs.fuzzel ];
}