{ lib, pkgs, ... }:
{
  home.packages = [ pkgs.heroic ];
  xdg.mimeApps.defaultApplications."x-scheme-handler/heroic" = [ "heroic.desktop" ];
}