{ lib, pkgs, config, self, osConfig, ... }:

let
  inherit (config.custom.desktop-environment) apps;

  cfg = config.custom.desktop-environment.menus;

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };
in
{
  options.custom.desktop-environment.menus = {
    session = mkAppOpt' (self.lib.builders.writeDmenuScript pkgs {
      name = "session-dmenu";
      entries = [
        { label = "    Lock";                exec = apps.session.lock; }
        { label = "󰤄    Suspend";             exec = apps.session.suspend; }
        { label = "    Shutdown";            exec = apps.session.shutdown; }
        { label = "    Reboot";              exec = apps.session.reboot; }
        { label = "    Reboot to EFI setup"; exec = apps.session.reboot-efi; }
      ] ++ lib.optionals (osConfig.custom.boot.grub.windows.efiDevice != "") [
        { label = "    Reboot to Windows";   exec = osConfig.custom.boot.grub.windows.rebootPackage; }
      ];
    });
    screenshot      = mkAppOpt { };
  };
}
