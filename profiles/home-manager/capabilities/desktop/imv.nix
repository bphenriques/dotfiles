{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.imv.enable = true;
  custom.xdgDefaultApps.image = lib.mkBefore [ "imv.desktop" ];
}
