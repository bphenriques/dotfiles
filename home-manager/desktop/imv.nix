{ pkgs, lib, config, ... }:
{
  programs.imv.enable = pkgs.stdenv.isLinux;
  custom.xdgDefaultApps.image = lib.mkBefore [ "imv.desktop" ];
}
