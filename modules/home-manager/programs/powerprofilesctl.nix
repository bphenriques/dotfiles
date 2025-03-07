{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.programs.powerprofilesctl;

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
in
{
  options.custom.programs.powerprofilesctl.enable = lib.mkEnableOption "custom-powerprofilesctl";

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.custom-powerprofilesctl" pkgs lib.platforms.linux) ];
    custom.programs.wlr-which-key.menus.powerprofilesctl = {
      p = cmd "Performance" "powerprofilesctl-notify set performance";
      b = cmd "Balanced" "powerprofilesctl-notify set balanced";
      s = cmd "Power Saver" "powerprofilesctl-notify set power-saver";
      g = cmd "Get" "powerprofilesctl-notify get";
    };

    home.packages = [
      self.pkgs.powerprofilesctl-notify
    ];
  };
}
