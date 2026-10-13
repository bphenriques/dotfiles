{ config, pkgs, lib, ... }:
{
   home.packages = [
    pkgs.awscli2
  ];

  home.stateVersion = "26.05";
}
