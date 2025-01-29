{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "volume-osd";
  runtimeInputs = [ pkgs.libnotify pkgs.ponymix ];
  text = let
    iconBasePath = "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status";
  in
  ''
    OSD_VOLUME_MUTED_ICON="${iconBasePath}/audio-volume-muted-symbolic.svg"
    OSD_VOLUME_LOW_ICON="${iconBasePath}/audio-volume-low-symbolic.svg"
    OSD_VOLUME_MEDIUM_ICON="${iconBasePath}/audio-volume-medium-symbolic.svg"
    OSD_VOLUME_HIGH_ICON="${iconBasePath}/audio-volume-high-symbolic.svg"

    ${lib.fileContents ./script.sh}
  '';
}