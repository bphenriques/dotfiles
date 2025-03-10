{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.programs.powerprofilesctl;

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };

  powerprofilesctl-notify = lib.getExe cfg.package;
in
{
  options.custom.programs.powerprofilesctl = {
    enable = lib.mkEnableOption "custom-powerprofilesctl";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.powerprofilesctl-notify;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.powerprofilesctl" pkgs lib.platforms.linux) ];
    custom.programs.wlr-which-key.menus.powerprofilesctl = {
      g = cmd "Get" "${powerprofilesctl-notify} get";
      s = cmd "Power Saver" "${powerprofilesctl-notify} set power-saver";
      b = cmd "Balanced" "${powerprofilesctl-notify} set balanced";
      p = cmd "Performance" "${powerprofilesctl-notify} set performance";
    };

    home.packages = [ cfg.package ];
  };
}
