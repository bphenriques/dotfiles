{ config, lib, pkgs, ... }:
{
  custom = {
    lutris.enable = true;
    steam.enable = true;
    proton-run = {
      enable = true;
      defaultProtonDir = "/mnt/games/GlobalProton";
    };
  };

  xdg.mimeApps.defaultApplications."x-scheme-handler/heroic" = [ "heroic.desktop" ];
}



