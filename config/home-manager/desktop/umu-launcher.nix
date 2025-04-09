{ lib, config, pkgs, ... }:
let
  umu-run0 = pkgs.writeShellApplication {
    name = "umu-run0";
    text = ''WINEPREFIX=$XDG_DATA_HOME/umu-launcher-games GAMEID=0 PROTONPATH=GE-Proton ${lib.getExe' pkgs.umu-launcher "umu-run"} "$@"'';
  };

  umu-run0-desktop-item =
    (pkgs.makeDesktopItem {
      name = "umu-launcher0";
      exec = "${lib.getExe umu-run0} %f";
      type = "Application";
      desktopName = "umu-launcher0";
      categories = [ "Utility" "Game" ];
      icon = "wine";
      mimeTypes = mimeTypes;
      extraConfig.Terminal = "true";
    });

  mimeTypes = [
    "application/x-ms-dos-executable"
    "application/x-msi"
    "application/x-ms-shortcut"
    "application/vnd.microsoft.portable-executable"
  ];

  setDefault = types: target: lib.foldl' (acc: type: acc // { "${type}" = target; }) { } types;
in lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [
    pkgs.umu-launcher
    umu-run0
    umu-run0-desktop-item
  ];

  xdg.mimeApps.defaultApplications = setDefault mimeTypes "umu-launcher0.desktop";
}