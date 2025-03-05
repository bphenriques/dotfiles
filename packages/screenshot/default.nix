{
  lib,
  pkgs,
  errorIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/dialog-error-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "screenshot";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.grim
    pkgs.swappy
    pkgs.slurp
    pkgs.libnotify
    pkgs.wl-clipboard
  ];
  text = ''
    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}