{ config, pkgs, lib, ... }:
{
  home.packages = [ pkgs.fuzzel ];
}