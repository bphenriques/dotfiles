{
  lib,
  pkgs,
  ...
}:
pkgs.writeShellApplication {
  name = "screenshot";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.grim
    pkgs.satty
    pkgs.slurp
    pkgs.libnotify
    pkgs.wl-clipboard
    pkgs.niri
  ];
  text = ''
    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}