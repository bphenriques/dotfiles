{ lib, pkgs, config, osConfig, self, ... }:
let
  cfg = config.custom.programs.brightness-osd;

  brightness-osd = lib.getExe cfg.package;
  mkIcon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; };
in
{
  options.custom.programs.brightness-osd = {
    enable = lib.mkEnableOption "custom-brightness-osd";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.brightness-osd.override {
        iconOff = mkIcon "brightness-osd-off" "󰃞";
        iconLow = mkIcon "brightness-osd-low" "󰃞";
        iconMedium = mkIcon "brightness-osd-medium" "󰃟";
        iconHigh = mkIcon "brightness-osd-high" "󰃠";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "custom.programs.custom-brightness-osd" pkgs lib.platforms.linux)
    ];

    custom.programs.wlr-which-key.menus.brightness-osd = [
      { key = ["Up" "k"];   desc = "Increase brightness"; cmd = "${brightness-osd} increase"; keep_open = true; }
      { key = ["Down" "j"]; desc = "Reduce brightness";   cmd = "${brightness-osd} decrease"; keep_open = true; }
    ];

    home.packages = [
      cfg.package
      pkgs.brightnessctl
    ];
  };
}

