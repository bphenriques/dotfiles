{ pkgs, lib, ... }:
{
  programs.imv.enable = true;
  my.xdgDefaultApps.image = lib.mkBefore [ "imv.desktop" ];
}
