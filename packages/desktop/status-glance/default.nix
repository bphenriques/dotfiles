{
  lib,
  pkgs,
  upower-notify,
  volume-osd,
  niri-keyboard-layout,
  upowerDevice ? "battery_BAT0",
  upowerNotifyLow ? 30,
  upowerNotifyCritical ? 20,
  ...
}:
pkgs.writeShellApplication {
  name = "status-glance";
  runtimeInputs = [
    upower-notify
    volume-osd
    niri-keyboard-layout
    pkgs.coreutils
    pkgs.libnotify
    pkgs.networkmanager
  ];
  text = ''
    export UPOWER_DEVICE="${upowerDevice}"
    export UPOWER_NOTIFY_LOW="${toString upowerNotifyLow}"
    export UPOWER_NOTIFY_CRITICAL="${toString upowerNotifyCritical}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}
