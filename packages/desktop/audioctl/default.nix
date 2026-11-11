{
  lib,
  pkgs,
  headphonesIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-headphones.svg",
  headsetIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-headset.svg",
  internalSpeakersIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-card.svg",
  externalSpeakersIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/32x32/devices/audio-speakers.svg",
  microphoneIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/48x48/status/notification-microphone-sensitivity-high.svg",
  errorIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/dialog-error-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "audioctl";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.pulseaudio
    pkgs.wireplumber
    pkgs.jq
    pkgs.libnotify
    pkgs.fuzzel
  ];
  text = ''
    # Variables are accessed via indirect expansion (''${!var}) in get_icon
    # shellcheck disable=SC2034
    AUDIOCTL_HEADPHONES_ICON="${headphonesIcon}"
    # shellcheck disable=SC2034
    AUDIOCTL_HEADSET_ICON="${headsetIcon}"
    # shellcheck disable=SC2034
    AUDIOCTL_INTERNAL_SPEAKERS_ICON="${internalSpeakersIcon}"
    # shellcheck disable=SC2034
    AUDIOCTL_EXTERNAL_SPEAKERS_ICON="${externalSpeakersIcon}"
    # shellcheck disable=SC2034
    AUDIOCTL_MICROPHONE_ICON="${microphoneIcon}"
    # shellcheck disable=SC2034
    AUDIOCTL_ERROR_ICON="${errorIcon}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}