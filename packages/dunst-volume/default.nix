{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "dunst-volume";
  runtimeInputs = with pkgs; [ dunst ponymix ];
  text = let
    iconBasePath = "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status";
  in
  ''
    DUNST_VOLUME_MUTED_ICON="${iconBasePath}/audio-volume-muted-symbolic.svg"
    DUNST_VOLUME_LOW_ICON="${iconBasePath}/audio-volume-low-symbolic.svg"
    DUNST_VOLUME_MEDIUM_ICON="${iconBasePath}/audio-volume-medium-symbolic.svg"
    DUNST_VOLUME_HIGH_ICON="${iconBasePath}/audio-volume-high-symbolic.svg"

    ${lib.fileContents ./src/dunst-volume.sh}
  '';
}