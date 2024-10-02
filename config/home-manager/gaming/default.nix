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
}



