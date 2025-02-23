{ lib, pkgs, config, self, osConfig, ... }:

let
  cfg = config.custom.desktop-environment.picker;

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };

  dmenu = "${lib.getExe pkgs.fuzzel} -d";
in
{
  options.custom.desktop-environment.picker = {
    application   = mkAppOpt' pkgs.fuzzel;
    dmenu         = mkAppOpt' dmenu;
    emoji         = mkAppOpt' (pkgs.writeShellApplication {
      name = "emoji-picker";
      runtimeInputs = [ pkgs.wtype ];
      text = ''BEMOJI_PICKER_CMD="${dmenu}" ${lib.getExe pkgs.bemoji} --noline --type --clip'';
    });
  };

  config = {
    programs.fuzzel.settings.main.terminal = config.custom.desktop-environment.terminal.emulator;
  };
}
