{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.desktop-environment.compositor;

  niri = lib.getExe pkgs.niri;

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };
in
{
  options.custom.desktop-environment.compositor = {
    window-switcher     = mkAppOpt { };
    focused-output      = mkAppOpt { };
    power-off-monitors  = mkAppOpt { };
    power-on-monitors   = mkAppOpt { };
  };

  config = {
    custom.desktop-environment = lib.mkIf (config.wayland.systemd.target == "niri.service") {
      session.logout = "${niri} msg action quit";
      compositor = {
        window-switcher = lib.getExe self.pkgs.niri-window-dmenu;
        focused-output = "${niri} msg --json focused-output | ${lib.getExe pkgs.jq} -r '.name'";
        power-off-monitors = "${niri} msg action power-off-monitors";
        power-on-monitors = "${niri} msg action power-on-monitors";
      };
    };
  };
}
