{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.imv.enable = true;
  my.xdgDefaultApps.image = lib.mkBefore [ "imv.desktop" ];
}
