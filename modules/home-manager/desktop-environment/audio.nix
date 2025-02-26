{ config, self, lib, ... }:
let
  mkAppOpt = default: lib.mkOption {
    inherit default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  deviceOpt = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
  };

  volume = lib.getExe self.pkgs.volume-osd;
in
{
  options.custom.desktop-environment.audio = {
    increase    = mkAppOpt "${volume} increase";
    decrease    = mkAppOpt "${volume} decrease";
    toggle-mute = mkAppOpt "${volume} toggle-mute";
  };
}

#headset = {
#  name = "alsa_output.usb-SteelSeries_SteelSeries_Arctis_7-00.stereo-game";
#  normal = "";
#  muted = "󰋐";
#};
#
#external-speaker = {
#  name = "alsa_output.pci-0000_01_00.1.hdmi-stereo";
#  normal = "󰓃";
#  muted = "󰓄";
#};
#
#internal-speaker = {
#  name = "alsa_output.pci-0000_06_00.6.analog-stereo";
#  normal = "󰽟";
#  muted = "󰽠";
#};