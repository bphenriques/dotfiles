{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ (pkgs.discord.override { branch = "canary"; }) ];
  xdg.mimeApps.defaultApplications."x-scheme-handler/discord" = [ "discord.desktop" ];
}