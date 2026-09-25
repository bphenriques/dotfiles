{ lib, pkgs, config, self, ... }:
let
  cfg = config.my.programs.status-glance;
  upowerCfg = config.my.services.upower-notify;
in
{
  options.my.programs.status-glance = {
    enable = lib.mkEnableOption "status-glance";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.status-glance.override {
        upower-notify = upowerCfg.package;
        volume-osd = config.my.programs.volume-osd.package;
        niri-keyboard-layout = config.my.programs.niri-keyboard-layout.package;
        upowerDevice = upowerCfg.device;
        upowerNotifyLow = upowerCfg.percentageLow;
        upowerNotifyCritical = upowerCfg.percentageCritical;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "my.programs.status-glance" pkgs lib.platforms.linux)
      { assertion = upowerCfg.enable; message = "status-glance requires my.services.upower-notify to be enabled"; }
      { assertion = config.my.programs.volume-osd.enable; message = "status-glance requires my.programs.volume-osd to be enabled"; }
      { assertion = config.my.programs.niri-keyboard-layout.enable; message = "status-glance requires my.programs.niri-keyboard-layout to be enabled"; }
    ];

    home.packages = [ cfg.package ];
  };
}
