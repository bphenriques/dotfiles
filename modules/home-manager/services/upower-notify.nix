{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.services.upower-notify;

  mkIcon = text: icon: textColor: self.lib.builders.mkNerdFontIcon { inherit textColor; } text icon;

  batteryLevels = [
    { level = 0;   glyph = "󰂎"; chargingGlyph = "󰢟"; color = config.lib.stylix.colors.withHashtag.base08; }
    { level = 10;  glyph = "󰁺"; chargingGlyph = "󰢜"; color = config.lib.stylix.colors.withHashtag.base08; }
    { level = 20;  glyph = "󰁻"; chargingGlyph = "󰂆"; color = config.lib.stylix.colors.withHashtag.base0A; }
    { level = 30;  glyph = "󰁼"; chargingGlyph = "󰂇"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 40;  glyph = "󰁽"; chargingGlyph = "󰂈"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 50;  glyph = "󰁾"; chargingGlyph = "󰢝"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 60;  glyph = "󰁿"; chargingGlyph = "󰂉"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 70;  glyph = "󰂀"; chargingGlyph = "󰢞"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 80;  glyph = "󰂁"; chargingGlyph = "󰂊"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 90;  glyph = "󰂂"; chargingGlyph = "󰂋"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 100; glyph = "󰁹"; chargingGlyph = "󰂅"; color = config.lib.stylix.colors.withHashtag.base0B; }
  ];

  mkLevelIcons = mkIconFn: builtins.listToAttrs (map (b: {
    name = toString b.level;
    value = mkIconFn b;
  }) batteryLevels);
in
{
  options.custom.services.upower-notify = {
    enable = lib.mkEnableOption "upower-notify";

    device = lib.mkOption {
      type = lib.types.str;
      default = "battery_BAT0";
      description = "UPower battery device name (see: upower -e)";
    };

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
      default = self.packages.upower-notify.override {
        icons = mkLevelIcons (b: mkIcon "battery${toString b.level}" b.glyph b.color);
        chargingIcons = mkLevelIcons (b: mkIcon "battery${toString b.level}-charging" b.chargingGlyph config.lib.stylix.colors.withHashtag.base0B);
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "custom.services.upower-notify" pkgs lib.platforms.linux)
      {
        assertion = cfg.percentageCritical < cfg.percentageLow;
        message = "custom.services.upower-notify: percentageCritical (${toString cfg.percentageCritical}) must be less than percentageLow (${toString cfg.percentageLow})";
      }
    ];
    systemd.user.services.upower-notify = {
      Unit = {
        Description = "upower battery monitor";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        Type = "simple";
        Restart = "on-failure";
        RestartSec = "5s";
        Environment = [
          "UPOWER_DEVICE=${cfg.device}"
          "UPOWER_NOTIFY_LOW=${toString cfg.percentageLow}"
          "UPOWER_NOTIFY_CRITICAL=${toString cfg.percentageCritical}"
        ];
        ExecStart = ''${lib.getExe cfg.package} monitor'';
      };
    };
  };
}
