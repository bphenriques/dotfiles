{
  lib,
  pkgs,
  headphonesIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-headphones.svg",
  headphonesMuteIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-headphones.svg",
  headsetIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-headset.svg",
  headsetMuteIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-headset.svg",
  internalSpeakersIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-card.svg",
  internalSpeakersMuteIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-card.svg",
  externalSpeakersIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-speakers.svg",
  externalSpeakersMuteIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-speakers.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "volume-osd";
  runtimeInputs = [
    pkgs.pulseaudio
    pkgs.jq
    pkgs.libnotify
  ];
  text = ''
    OSD_VOLUME_HEADPHONES_ICON="${headphonesIcon}"
    OSD_VOLUME_HEADPHONES_MUTE_ICON="${headphonesMuteIcon}"
    OSD_VOLUME_HEADSET_ICON="${headsetIcon}"
    OSD_VOLUME_HEADSET_MUTE_ICON="${headsetMuteIcon}"
    OSD_VOLUME_INTERNAL_SPEAKERS_ICON="${internalSpeakersIcon}"
    OSD_VOLUME_INTERNAL_SPEAKERS_MUTE_ICON="${internalSpeakersMuteIcon}"
    OSD_VOLUME_EXTERNAL_SPEAKERS_ICON="${externalSpeakersIcon}"
    OSD_VOLUME_EXTERNAL_SPEAKERS_MUTE_ICON="${externalSpeakersMuteIcon}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}