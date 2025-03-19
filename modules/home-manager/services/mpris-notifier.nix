{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.services.mpris-notifier;

  tomlFormat = pkgs.formats.toml { };
in
{
  options.custom.services.mpris-notifier = {
    enable = lib.mkEnableOption "mpris-notifier";
    settings = lib.mkOption {
      type = tomlFormat.type;
      default = { };
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.services.mpris-notifier" pkgs lib.platforms.linux) ];

    xdg.configFile."mpris-notifier/config.toml" = lib.mkIf (cfg.settings != { }) {
      source = tomlFormat.generate "config.toml" cfg.settings;
    };

    systemd.user.services = {
      mpris-notifier = {
        Unit = {
          Description = "mpris notifier";
          PartOf = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
        Install.WantedBy = [ config.wayland.systemd.target ];
        Service = {
          Type = "simple";
          ExecStart = ''${lib.getExe pkgs.mpris-notifier}'';
          Restart = "on-failure";
        };
      };
    };
  };
}