{ lib, pkgs, config, self, ... }:
let
  cfg = config.my.services.upower-notify;

  mkIcon = text: icon: textColor: self.lib.builders.mkNerdFontIcon { inherit textColor; } text icon;

  batteryLevels = [
    { level = 0;   glyph = self.lib.glyphs."md-battery_outline"; chargingGlyph = self.lib.glyphs."md-battery_charging_outline"; color = config.lib.stylix.colors.withHashtag.base08; }
    { level = 10;  glyph = self.lib.glyphs."md-battery_10"; chargingGlyph = self.lib.glyphs."md-battery_charging_10"; color = config.lib.stylix.colors.withHashtag.base08; }
    { level = 20;  glyph = self.lib.glyphs."md-battery_20"; chargingGlyph = self.lib.glyphs."md-battery_charging_20"; color = config.lib.stylix.colors.withHashtag.base0A; }
    { level = 30;  glyph = self.lib.glyphs."md-battery_30"; chargingGlyph = self.lib.glyphs."md-battery_charging_30"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 40;  glyph = self.lib.glyphs."md-battery_40"; chargingGlyph = self.lib.glyphs."md-battery_charging_40"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 50;  glyph = self.lib.glyphs."md-battery_50"; chargingGlyph = self.lib.glyphs."md-battery_charging_50"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 60;  glyph = self.lib.glyphs."md-battery_60"; chargingGlyph = self.lib.glyphs."md-battery_charging_60"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 70;  glyph = self.lib.glyphs."md-battery_70"; chargingGlyph = self.lib.glyphs."md-battery_charging_70"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 80;  glyph = self.lib.glyphs."md-battery_80"; chargingGlyph = self.lib.glyphs."md-battery_charging_80"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 90;  glyph = self.lib.glyphs."md-battery_90"; chargingGlyph = self.lib.glyphs."md-battery_charging_90"; color = config.lib.stylix.colors.withHashtag.base0B; }
    { level = 100; glyph = self.lib.glyphs."md-battery"; chargingGlyph = self.lib.glyphs."md-battery_charging_100"; color = config.lib.stylix.colors.withHashtag.base0B; }
  ];

  mkLevelAttrs = fn: builtins.listToAttrs (map (b: {
    name = toString b.level;
    value = fn b;
  }) batteryLevels);
in
{
  options.my.services.upower-notify = {
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
        icons = mkLevelAttrs (b: mkIcon "battery${toString b.level}" b.glyph b.color);
        chargingIcons = mkLevelAttrs (b: mkIcon "battery${toString b.level}-charging" b.chargingGlyph config.lib.stylix.colors.withHashtag.base0B);
      };
    };
  };

  config = lib.mkIf cfg.enable {
    my.programs.desktop-shell.settings.glyphs.battery = {
      discharging = lib.map (b: b.glyph) batteryLevels;
      charging = lib.map (b: b.chargingGlyph) batteryLevels;
    };

    assertions = [
      (lib.hm.assertions.assertPlatform "my.services.upower-notify" pkgs lib.platforms.linux)
      {
        assertion = cfg.percentageCritical < cfg.percentageLow;
        message = "my.services.upower-notify: percentageCritical (${toString cfg.percentageCritical}) must be less than percentageLow (${toString cfg.percentageLow})";
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
