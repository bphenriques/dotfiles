{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.services.mpc-plus;

  tomlFormat = pkgs.formats.toml { };
in
{
  options.custom.services.mpc-plus = {
    enable = lib.mkEnableOption "mpc-plus";
    package = lib.mkOption {
      type = lib.types.package;
      default = config.custom.programs.mpc-plus.package;
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.mpc-plus" pkgs lib.platforms.linux) ];
    systemd.user.services.mpc-plus = {
      Unit = {
        Description = "mpc-plus notifications daemon";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = ''${lib.getExe cfg.package} notifications-daemon'';
        Restart = "on-failure";
      };
    };
  };
}