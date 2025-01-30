{ lib, pkgs, ... }:
let
  umu-run0 = pkgs.writeShellApplication {
    name = "umu-run0";
    text = ''GAMEID=0 ${lib.getExe pkgs.umu-run}'';
  };

  mimeType = [
    "application/x-ms-dos-executable"
    "application/x-msi"
    "application/x-ms-shortcut"
    "application/vnd.microsoft.portable-executable"
  ];

  setDefault = types: target: builtins.listToAttrs (mime: { "${mime}" = target; }) types;
in lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [
    umu-run0
    pkgs.umu-launcher
  ];

  xdg = {
    desktopEntries."Umu Default Launcher" = {
      inherit mimeType;
      exec = "${lib.getExe umu-run0} %f";
      icon = "wine";
    };
    mime.defaultApplications = setDefault mimeTypes "Umu Default Launcher.desktop";
  };
}