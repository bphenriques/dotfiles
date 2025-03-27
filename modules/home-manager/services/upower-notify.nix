{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.services.upower-notify;

  mkIcon = text: icon: textColor: self.lib.builders.mkNerdFontIcon pkgs { inherit textColor; } text icon;
in
{
  options.custom.services.upower-notify = {
    enable = lib.mkEnableOption "upower-notify";

    percentageCritical = lib.mkOption {
      type = lib.types.int;
      default = 20;
    };

    percentageLow = lib.mkOption {
      type = lib.types.int;
      default = 30;
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.upower-notify.override {
        battery0Icon            = mkIcon "battery0" "󰂎" config.lib.stylix.colors.withHashtag.base08;
        battery0ChargingIcon    = mkIcon "battery0-charging" "󰢟" config.lib.stylix.colors.withHashtag.base0B;
        battery10Icon           = mkIcon "battery10" "󰁺" config.lib.stylix.colors.withHashtag.base08;
        battery10ChargingIcon   = mkIcon "battery10-charging" "󰢜" config.lib.stylix.colors.withHashtag.base0B;
        battery20Icon           = mkIcon "battery20" "󰁻" config.lib.stylix.colors.withHashtag.base0A;
        battery20ChargingIcon   = mkIcon "battery20-charging" "󰂆" config.lib.stylix.colors.withHashtag.base0B;
        battery30Icon           = mkIcon "battery30" "󰁼" config.lib.stylix.colors.withHashtag.base0B;
        battery30ChargingIcon   = mkIcon "battery30-charging" "󰂇" config.lib.stylix.colors.withHashtag.base0B;
        battery40Icon           = mkIcon "battery40" "󰁽" config.lib.stylix.colors.withHashtag.base0B;
        battery40ChargingIcon   = mkIcon "battery40-charging" "󰂈" config.lib.stylix.colors.withHashtag.base0B;
        battery50Icon           = mkIcon "battery50" "󰁾" config.lib.stylix.colors.withHashtag.base0B;
        battery50ChargingIcon   = mkIcon "battery50-charging" "󰢝" config.lib.stylix.colors.withHashtag.base0B;
        battery60Icon           = mkIcon "battery60" "󰁿" config.lib.stylix.colors.withHashtag.base0B;
        battery60ChargingIcon   = mkIcon "battery60-charging" "󰂉" config.lib.stylix.colors.withHashtag.base0B;
        battery70Icon           = mkIcon "battery70" "󰂀" config.lib.stylix.colors.withHashtag.base0B;
        battery70ChargingIcon   = mkIcon "battery70-charging" "󰢞" config.lib.stylix.colors.withHashtag.base0B;
        battery80Icon           = mkIcon "battery80" "󰂁" config.lib.stylix.colors.withHashtag.base0B;
        battery80ChargingIcon   = mkIcon "battery80-charging" "󰂊" config.lib.stylix.colors.withHashtag.base0B;
        battery90Icon           = mkIcon "battery90" "󰂂" config.lib.stylix.colors.withHashtag.base0B;
        battery90ChargingIcon   = mkIcon "battery90-charging" "󰂋" config.lib.stylix.colors.withHashtag.base0B;
        battery100Icon          = mkIcon "battery100" "󰁹" config.lib.stylix.colors.withHashtag.base0B;
        battery100ChargingIcon  = mkIcon "battery100-charging" "󰂅" config.lib.stylix.colors.withHashtag.base0B;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.upower-notify" pkgs lib.platforms.linux) ];
    systemd.user.services = {
      upower-notify = {
        Unit = {
          Description = "upower notifications";
          PartOf = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
        Install.WantedBy = [ "graphical-session.target" ];
        Service = {
          Type = "simple";
          Environment = [
            "UPOWER_NOTIFY_LOW=${toString cfg.percentageLow}"
            "UPOWER_NOTIFY_CRITICAL=${toString cfg.percentageCritical}"
          ];
          ExecStart = ''${lib.getExe cfg.package} monitor-status'';
          Restart = "on-failure";
        };
      };
    };
  };
}
