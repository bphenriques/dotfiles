{ lib, pkgs, config, self, osConfig, ... }:

let
  inherit (config.custom.desktop-environment) terminal;

  cfg = config.custom.desktop-environment.system;

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };
in
{
  options.custom.desktop-environment.system = {
    monitor        = mkAppOpt' ''${terminal.emulator} --title=btop-tui ${lib.getExe pkgs.btop}'';
  };

  config = {
    home.packages = [
      (pkgs.makeDesktopItem {
        name = "system-monitor";
        desktopName = "System Monitor";
        icon = "folder";  # FIXME
        exec =  cfg.monitor;
      })
    ];
  };
}
