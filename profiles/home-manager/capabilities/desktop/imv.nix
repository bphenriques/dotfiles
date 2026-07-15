{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.imv.enable = true;
  custom.xdgDefaultApps.image = lib.mkBefore [ "imv.desktop" ];
}
