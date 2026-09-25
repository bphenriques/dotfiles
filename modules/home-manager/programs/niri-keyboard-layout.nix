{ lib, pkgs, config, self, ... }:
let
  cfg = config.my.programs.niri-keyboard-layout;

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };
in
{
  options.my.programs.niri-keyboard-layout = {
    enable = lib.mkEnableOption "custom-niri-keyboard-layout";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.niri-keyboard-layout.override {
        keyboardIcon = mkIcon "keyboard-layout" "󰌌";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "my.programs.niri-keyboard-layout" pkgs lib.platforms.linux)
    ];

    home.packages = [
      cfg.package
    ];
  };
}
