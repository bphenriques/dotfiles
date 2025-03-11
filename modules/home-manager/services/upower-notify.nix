{ lib, pkgs, config, self, osConfig, ... }:
let
  cfg = config.custom.services.upower-notify;

  mkIcon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; };
in
{
  options.custom.services.upower-notify = {
    enable = lib.mkEnableOption "upower-notify";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.upower-notify.override {
        battery0Icon            = mkIcon "battery0" "󰂎";
        battery0ChargingIcon    = mkIcon "battery0-charging" "󰢟";
        battery10Icon           = mkIcon "battery10" "󰁺";
        battery10ChargingIcon   = mkIcon "battery10-charging" "󰢜";
        battery20Icon           = mkIcon "battery20" "󰁻";
        battery20ChargingIcon   = mkIcon "battery20-charging" "󰂆";
        battery30Icon           = mkIcon "battery30" "󰁼";
        battery30ChargingIcon   = mkIcon "battery30-charging" "󰂇";
        battery40Icon           = mkIcon "battery40" "󰁽";
        battery40ChargingIcon   = mkIcon "battery40-charging" "󰂈";
        battery50Icon           = mkIcon "battery50" "󰁾";
        battery50ChargingIcon   = mkIcon "battery50-charging" "󰢝";
        battery60Icon           = mkIcon "battery60" "󰁿";
        battery60ChargingIcon   = mkIcon "battery60-charging" "󰂉";
        battery70Icon           = mkIcon "battery70" "󰂀";
        battery70ChargingIcon   = mkIcon "battery70-charging" "󰢞";
        battery80Icon           = mkIcon "battery80" "󰂁";
        battery80ChargingIcon   = mkIcon "battery80-charging" "󰂊";
        battery90Icon           = mkIcon "battery90" "󰂂";
        battery90ChargingIcon   = mkIcon "battery90-charging" "󰂋";
        battery100Icon          = mkIcon "battery100" "󰁹";
        battery100ChargingIcon  = mkIcon "battery100-charging" "󰂅";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.upower-notify" pkgs lib.platforms.linux) ];
    systemd.user.services = {
      upower-notify = {
        Unit = {
          Description = "upower notifications";
          ConditionEnvironment = [ "WAYLAND_DISPLAY" ];
          PartOf = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
        Install.WantedBy = [ config.wayland.systemd.target ];
        Service = {
          Type = "simple";
          Environment = [
            "UPOWER_NOTIFY_LOW=${toString osConfig.services.upower.percentageLow}"
            "UPOWER_NOTIFY_CRITICAL=${toString osConfig.services.upower.percentageCritical}"
          ];
          ExecStart = ''${lib.getExe cfg.package} monitor-status'';
          Restart = "on-failure";
        };
      };
    };
  };
}