{ config, lib, pkgs, self, osConfig, ... }:
let
  inherit (config.custom.desktop-environment.apps) session screenshot scren-recorder;
in
{
  custom.desktop-environment.apps.menus = {
    session = self.lib.builders.writeDmenuScript pkgs {
      name = "session-dmenu";
      entries = [
        { label = "    Lock";                exec = session.lock; }
        { label = "󰤄    Suspend";             exec = session.suspend; }
        { label = "    Shutdown";            exec = session.shutdown; }
        { label = "    Reboot";              exec = session.reboot; }
        { label = "    Reboot to EFI setup"; exec = session.reboot-efi; }
      ] ++ lib.optionals (osConfig.custom.boot.grub.windows.efiDevice != "") [
        { label = "    Reboot to Windows";   exec = osConfig.custom.boot.grub.windows.rebootPackage; }
      ];
    };

    screenshot = self.lib.builders.writeDmenuScript pkgs {
      name = "screenshot-dmenu";
      entries = [
        { label = "󰹑    Screenshot screen";           exec = screenshot.screen; }
        { label = "󰹑    Screenshot screen (edit)";    exec = screenshot.screen-edit; }
        { label = "󰹑    Screenshot screen (copy)";    exec = screenshot.screen-copy; }
        { label = "    Screenshot region";           exec = screenshot.region; }
        { label = "    Screenshot region (edit)";    exec = screenshot.region-edit; }
        { label = "    Screenshot region (copy)";    exec = screenshot.region-copy; }
      ];
    };
  };
}