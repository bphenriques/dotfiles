{ lib, pkgs, config, self, osConfig, ... }:

let
  cfg = config.custom.desktop-environment.session;

  systemctl = lib.getExe' pkgs.systemd "systemctl";

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };
in
{
  options.custom.desktop-environment.session = {
    shutdown    = mkAppOpt' "${systemctl} poweroff";
    suspend     = mkAppOpt' "${systemctl} suspend";
    reboot      = mkAppOpt' "${systemctl} reboot";
    reboot-efi  = mkAppOpt' "${systemctl} reboot --firmware-setup";
    lock        = mkAppOpt { };
    logout      = mkAppOpt { };

    dmenu = mkAppOpt' (self.lib.builders.writeDmenuScript pkgs {
      name = "session-dmenu";
      entries = [
        { label = "    Lock";                exec = cfg.lock; }
        { label = "󰤄    Suspend";             exec = cfg.suspend; }
        { label = "    Shutdown";            exec = cfg.shutdown; }
        { label = "    Reboot";              exec = cfg.reboot; }
        { label = "    Reboot to EFI setup"; exec = cfg.reboot-efi; }
      ] ++ lib.optionals (osConfig.custom.boot.grub.windows.efiDevice != "") [
        { label = "    Reboot to Windows";   exec = osConfig.custom.boot.grub.windows.rebootPackage; }
      ];
    });
  };
}
