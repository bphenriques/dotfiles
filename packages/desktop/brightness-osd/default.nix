{
  lib,
  pkgs,
  iconOff ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/display-brightness-off-symbolic.svg",
  iconLow ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/display-brightness-low-symbolic.svg",
  iconMedium ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/display-brightness-medium-symbolic.svg",
  iconHigh ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/display-brightness-high-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "brightness-osd";
  runtimeInputs = [
    pkgs.libnotify
    pkgs.brightnessctl
    pkgs.gnugrep
    pkgs.gawk
    pkgs.findutils
  ];
  text = ''
    OSD_BRIGHTNESS_OFF_ICON="${iconOff}"
    OSD_BRIGHTNESS_LOW_ICON="${iconLow}"
    OSD_BRIGHTNESS_MEDIUM_ICON="${iconMedium}"
    OSD_BRIGHTNESS_HIGH_ICON="${iconHigh}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}