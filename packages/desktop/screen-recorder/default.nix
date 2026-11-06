{
  lib,
  pkgs,
  recordIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/media-record-symbolic.svg",
  informationIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/dialog-information-symbolic.svg",
  errorIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/dialog-error-symbolic.svg",
  audioDevice ? "@DEFAULT_MONITOR@",
  ...
}:
pkgs.writeShellApplication {
  name = "screen-recorder";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.procps
    pkgs.wl-screenrec
    pkgs.slurp
    pkgs.libnotify
    pkgs.ffmpeg-headless
    pkgs.xdg-utils
  ];
  text = ''
    RECORD_ICON="${recordIcon}"
    INFORMATION_ICON="${informationIcon}"
    ERROR_ICON="${errorIcon}"
    AUDIO_DEVICE="${audioDevice}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}
