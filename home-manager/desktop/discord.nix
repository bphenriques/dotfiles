{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.vesktop ]; # Lightweight discord
  xdg.mimeApps.defaultApplications."x-scheme-handler/discord" = [ "vesktop.desktop" ];
}