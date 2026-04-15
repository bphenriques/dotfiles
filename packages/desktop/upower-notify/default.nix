{
  lib,
  pkgs,
  icons ? builtins.listToAttrs (map (l: {
    name = toString l;
    value = "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-${toString l}-symbolic.svg";
  }) (builtins.genList (i: i * 10) 11)),
  chargingIcons ? builtins.listToAttrs (map (l: {
    name = toString l;
    value = "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-${toString l}-charging-symbolic.svg";
  }) (builtins.genList (i: i * 10) 11)),
  ...
}:
let
  iconsDir = pkgs.linkFarm "upower-icons" (lib.mapAttrsToList (name: path: { inherit name path; }) icons);
  chargingIconsDir = pkgs.linkFarm "upower-charging-icons" (lib.mapAttrsToList (name: path: { inherit name path; }) chargingIcons);
in
pkgs.writeShellApplication {
  name = "upower-notify";
  runtimeInputs = [
    pkgs.upower
    pkgs.acpi
    pkgs.libnotify
  ];
  text = ''
    UPOWER_ICONS_DIR="${iconsDir}"
    UPOWER_CHARGING_ICONS_DIR="${chargingIconsDir}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}
