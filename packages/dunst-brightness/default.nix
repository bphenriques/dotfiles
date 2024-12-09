{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "dunst-brightness";
  runtimeInputs = with pkgs; [ dunst brightnessctl ];
  text = let
    iconBasePath = "${pkgs.pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status";
  in
  # FIXME: This is not really configurable. Do something else about this.
  ''
    DUNST_BRIGHTNESS_OFF_ICON="${iconBasePath}/display-brightness-off-symbolic.svg"
    DUNST_BRIGHTNESS_LOW_ICON="${iconBasePath}/display-brightness-low-symbolic.svg"
    DUNST_BRIGHTNESS_MEDIUM_ICON="${iconBasePath}/display-brightness-medium-symbolic.svg"
    DUNST_BRIGHTNESS_HIGH_ICON="${iconBasePath}/display-brightness-high-symbolic.svg"

    ${lib.fileContents ./src/dunst-brightness.sh}
  '';
}