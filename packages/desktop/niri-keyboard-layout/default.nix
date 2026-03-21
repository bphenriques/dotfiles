{
  lib,
  pkgs,
  keyboardIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/devices/input-keyboard-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "niri-keyboard-layout";
  runtimeInputs = [
    pkgs.niri
    pkgs.jq
    pkgs.libnotify
  ];
  text = ''
    KEYBOARD_ICON="${keyboardIcon}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}