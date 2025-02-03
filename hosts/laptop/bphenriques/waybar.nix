{ lib, ... }:
let
  mkIcon = { name, normal, muted }: {
    "${name}" = normal;
    "${name}-muted" = muted;
  };

  headset = {
    name = "alsa_output.usb-SteelSeries_SteelSeries_Arctis_7-00.stereo-game";
    normal = "";
    muted = "󰋐";
  };

  external-speaker = {
    name = "alsa_output.pci-0000_01_00.1.hdmi-stereo";
    normal = "󰓃";
    muted = "󰓄";
  };

  internal-speaker = {
    name = "alsa_output.pci-0000_06_00.6.analog-stereo";
    normal = "󰽟";
    muted = "󰽠";
  };
in
{
  programs.waybar.settings.pulseaudio.format-icons = lib.mergeAttrsList [
    (mkIcon headset)
    (mkIcon external-speaker)
    (mkIcon internal-speaker)
  ];
}