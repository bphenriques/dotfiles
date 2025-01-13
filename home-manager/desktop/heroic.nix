{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.heroic ];
  xdg.mimeApps.defaultApplications."x-scheme-handler/heroic" = [ "heroic.desktop" ];
}