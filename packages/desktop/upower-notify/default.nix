{
  lib,
  pkgs,
  battery0Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-0-symbolic.svg",
  battery0ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-0-charging-symbolic.svg",
  battery10Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-10-symbolic.svg",
  battery10ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-10-charging-symbolic.svg",
  battery20Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-20-symbolic.svg",
  battery20ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-20-charging-symbolic.svg",
  battery30Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-30-symbolic.svg",
  battery30ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-30-charging-symbolic.svg",
  battery40Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-40-symbolic.svg",
  battery40ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-40-charging-symbolic.svg",
  battery50Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-50-symbolic.svg",
  battery50ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-50-charging-symbolic.svg",
  battery60Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-60-symbolic.svg",
  battery60ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-60-charging-symbolic.svg",
  battery70Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-70-symbolic.svg",
  battery70ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-70-charging-symbolic.svg",
  battery80Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-80-symbolic.svg",
  battery80ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-80-charging-symbolic.svg",
  battery90Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-90-symbolic.svg",
  battery90ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-90-charging-symbolic.svg",
  battery100Icon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-100-symbolic.svg",
  battery100ChargingIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/battery-level-100-charging-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "upower-notify";
  runtimeInputs = [
    pkgs.acpid
    pkgs.acpi
    pkgs.libnotify
  ];
  text = ''
    UPOWER_NOTIFY_LEVEL_0_ICON=${battery0Icon}
    UPOWER_NOTIFY_LEVEL_10_ICON=${battery10Icon}
    UPOWER_NOTIFY_LEVEL_20_ICON=${battery20Icon}
    UPOWER_NOTIFY_LEVEL_30_ICON=${battery30Icon}
    UPOWER_NOTIFY_LEVEL_40_ICON=${battery40Icon}
    UPOWER_NOTIFY_LEVEL_50_ICON=${battery50Icon}
    UPOWER_NOTIFY_LEVEL_60_ICON=${battery60Icon}
    UPOWER_NOTIFY_LEVEL_70_ICON=${battery70Icon}
    UPOWER_NOTIFY_LEVEL_80_ICON=${battery80Icon}
    UPOWER_NOTIFY_LEVEL_90_ICON=${battery90Icon}
    UPOWER_NOTIFY_LEVEL_100_ICON=${battery100Icon}
    UPOWER_NOTIFY_LEVEL_0_CHARGING_ICON=${battery0ChargingIcon}
    UPOWER_NOTIFY_LEVEL_10_CHARGING_ICON=${battery10ChargingIcon}
    UPOWER_NOTIFY_LEVEL_20_CHARGING_ICON=${battery20ChargingIcon}
    UPOWER_NOTIFY_LEVEL_30_CHARGING_ICON=${battery30ChargingIcon}
    UPOWER_NOTIFY_LEVEL_40_CHARGING_ICON=${battery40ChargingIcon}
    UPOWER_NOTIFY_LEVEL_50_CHARGING_ICON=${battery50ChargingIcon}
    UPOWER_NOTIFY_LEVEL_60_CHARGING_ICON=${battery60ChargingIcon}
    UPOWER_NOTIFY_LEVEL_70_CHARGING_ICON=${battery70ChargingIcon}
    UPOWER_NOTIFY_LEVEL_80_CHARGING_ICON=${battery80ChargingIcon}
    UPOWER_NOTIFY_LEVEL_90_CHARGING_ICON=${battery90ChargingIcon}
    UPOWER_NOTIFY_LEVEL_100_CHARGING_ICON=${battery100ChargingIcon}

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}