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
  microphoneIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/48x48/status/notification-microphone-sensitivity-high.svg",
  microphoneMuteIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/48x48/status/notification-microphone-sensitivity-muted.svg",
  errorIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/dialog-error-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "volume-osd";
  runtimeInputs = [
    pkgs.pulseaudio
    pkgs.wireplumber
    pkgs.jq
    pkgs.libnotify
    pkgs.fuzzel
  ];
  text = ''
    # Variables are accessed via indirect expansion (''${!var}) in get_icon
    # shellcheck disable=SC2034
    OSD_VOLUME_HEADPHONES_ICON="${headphonesIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_HEADPHONES_MUTE_ICON="${headphonesMuteIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_HEADSET_ICON="${headsetIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_HEADSET_MUTE_ICON="${headsetMuteIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_INTERNAL_SPEAKERS_ICON="${internalSpeakersIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_INTERNAL_SPEAKERS_MUTE_ICON="${internalSpeakersMuteIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_EXTERNAL_SPEAKERS_ICON="${externalSpeakersIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_EXTERNAL_SPEAKERS_MUTE_ICON="${externalSpeakersMuteIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_MICROPHONE_ICON="${microphoneIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_MICROPHONE_MUTE_ICON="${microphoneMuteIcon}"
    # shellcheck disable=SC2034
    OSD_VOLUME_ERROR_ICON="${errorIcon}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}