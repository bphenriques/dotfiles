{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "brightness-osd";
  runtimeInputs = with pkgs; [ libnotify brightnessctl gnugrep gawk findutils ];
  text = let
    iconBasePath = "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status";
  in
  # FIXME: This is not really configurable. Do something else about this.
  ''
    OSD_BRIGHTNESS_OFF_ICON="${iconBasePath}/display-brightness-off-symbolic.svg"
    OSD_BRIGHTNESS_LOW_ICON="${iconBasePath}/display-brightness-low-symbolic.svg"
    OSD_BRIGHTNESS_MEDIUM_ICON="${iconBasePath}/display-brightness-medium-symbolic.svg"
    OSD_BRIGHTNESS_HIGH_ICON="${iconBasePath}/display-brightness-high-symbolic.svg"

    ${lib.fileContents ./script.sh}
  '';
}