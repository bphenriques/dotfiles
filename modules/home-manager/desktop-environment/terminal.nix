{ lib, pkgs, config, self, osConfig, ... }:

let
  cfg = config.custom.desktop-environment.terminal;

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };
in
{
  options.custom.desktop-environment.terminal = {
    emulator = mkAppOpt' (lib.getExe' config.programs.foot.package "footclient");
  };
}
