{ lib, pkgs, ... }:
let
  umu-run0 = pkgs.writeShellApplication {
    name = "umu-run0";
    text = ''GAMEID=0 ${lib.getExe pkgs.umu-run}'';
  };

  mimeTypes = [
    "application/x-ms-dos-executable"
    "application/x-msi"
    "application/x-ms-shortcut"
    "application/vnd.microsoft.portable-executable"
  ];

  umu-run-default-desktop =
    (pkgs.makeDesktopItem {
      exec = "${lib.getExe umu-run0} %f";
      name = "Umu Default Launcher";
      type = "Application";
      desktopName = "Umu Default Launcher";
      categories = [ "Utility" "Game" ];
      icon = "wine";
      mimeTypes = mimeTypes;
    });

  setDefault = types: target: foldl' (acc: type: acc // { "${type}" = target; }) { } types;
in lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [
    umu-run0
    pkgs.umu-launcher
    umu-run-default-desktop
  ];

  xdg.mime.defaultApplications = setDefault mimeTypes "Umu Default Launcher.desktop";
}