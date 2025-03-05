{
  lib,
  pkgs,
  powerSaverIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/power-profile-power-saver-symbolic.svg",
  balancedIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/power-profile-balanced-symbolic.svg",
  performanceIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/power-profile-performance-symbolic.svg",
  errorIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/dialog-error-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "powerprofilesctl-notify";
  runtimeInputs = [
    pkgs.power-profiles-daemon
    pkgs.libnotify
  ];
  text = ''
    POWER_PROFILE_POWER_SAVER_ICON="${powerSaverIcon}"
    POWER_PROFILE_BALANCED_ICON="${balancedIcon}"
    POWER_PROFILE_PERFORMANCE_ICON="${performanceIcon}"
    POWER_PROFILE_ERROR_ICON="${errorIcon}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}
