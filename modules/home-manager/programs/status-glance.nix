{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.programs.status-glance;
  upowerCfg = config.custom.services.upower-notify;
in
{
  options.custom.programs.status-glance = {
    enable = lib.mkEnableOption "status-glance";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.status-glance.override {
        upower-notify = upowerCfg.package;
        volume-osd = config.custom.programs.volume-osd.package;
        niri-keyboard-layout = config.custom.programs.niri-keyboard-layout.package;
        upowerDevice = upowerCfg.device;
        upowerNotifyLow = upowerCfg.percentageLow;
        upowerNotifyCritical = upowerCfg.percentageCritical;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "custom.programs.status-glance" pkgs lib.platforms.linux)
      { assertion = upowerCfg.enable; message = "status-glance requires custom.services.upower-notify to be enabled"; }
      { assertion = config.custom.programs.volume-osd.enable; message = "status-glance requires custom.programs.volume-osd to be enabled"; }
      { assertion = config.custom.programs.niri-keyboard-layout.enable; message = "status-glance requires custom.programs.niri-keyboard-layout to be enabled"; }
    ];

    home.packages = [ cfg.package ];
  };
}
