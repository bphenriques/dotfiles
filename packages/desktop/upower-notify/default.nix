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
  dischargeGlyphs ? {},
  chargeGlyphs ? {},
  ...
}:
let
  mkIconDir = name: attrs: pkgs.linkFarm name (lib.mapAttrsToList (n: path: { name = n; path = path; }) attrs);
  mkBashAssocArray = attrs: lib.concatStringsSep " " (lib.mapAttrsToList (k: v: ''[${k}]="${v}"'') attrs);
in
pkgs.writeShellApplication {
  name = "upower-notify";
  runtimeInputs = [
    pkgs.upower
    pkgs.libnotify
  ];
  text = ''
    UPOWER_DISCHARGE_ICON_DIR="${mkIconDir "upower-discharge-icons" icons}"
    UPOWER_CHARGE_ICON_DIR="${mkIconDir "upower-charge-icons" chargingIcons}"
    declare -A UPOWER_DISCHARGE_GLYPH=(${mkBashAssocArray dischargeGlyphs})
    declare -A UPOWER_CHARGE_GLYPH=(${mkBashAssocArray chargeGlyphs})

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}
