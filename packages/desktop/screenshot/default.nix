{
  lib,
  pkgs,
  ...
}:
pkgs.writeShellApplication {
  name = "screenshot";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.gnugrep
    pkgs.grim
    pkgs.satty
    pkgs.slurp
    pkgs.libnotify
    pkgs.wl-clipboard
    pkgs.niri
    pkgs.xdg-utils
    pkgs.glib
  ];
  text = ''
    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}