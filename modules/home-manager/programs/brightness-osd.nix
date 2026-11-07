{ lib, pkgs, config, self, ... }:
let
  cfg = config.my.programs.brightness-osd;

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  glyphs = {
    low = "󰃞";
    medium = "󰃟";
    high = "󰃠";
  };
in
{
  options.my.programs.brightness-osd = {
    enable = lib.mkEnableOption "custom-brightness-osd";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.brightness-osd.override {
        iconOff = mkIcon "brightness-osd-off" glyphs.low;
        iconLow = mkIcon "brightness-osd-low" glyphs.low;
        iconMedium = mkIcon "brightness-osd-medium" glyphs.medium;
        iconHigh = mkIcon "brightness-osd-high" glyphs.high;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "my.programs.custom-brightness-osd" pkgs lib.platforms.linux)
    ];

    home.packages = [
      cfg.package
      pkgs.brightnessctl
    ];

    my.programs.desktop-shell.settings.glyphs.brightness = glyphs;

    my.programs.desktop-shell.menu.entries = lib.mkOrder 250 [
      { key = "l"; desc = "Brightness"; status = "brightness"; submenu = [
        { key = "Up";   desc = "Increase brightness"; cmd = "${lib.getExe cfg.package} increase"; keepOpen = true; }
        { key = "Down"; desc = "Reduce brightness";   cmd = "${lib.getExe cfg.package} decrease"; keepOpen = true; }
      ]; }
    ];
  };
}

