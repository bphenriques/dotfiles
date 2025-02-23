{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.desktop-environment.wallpaper;
in
{
  options.custom.desktop-environment.wallpaper = {
    directory = lib.mkOption {
      description = "Location of wallpaper";
      type = lib.types.str;
    };
  };

  config = {
    home.packages = [ self.pkgs.swww-util ];

    systemd.user.services.set-wallpaper = {
      Unit = {
        Description = "Sets the wallpaper";
        PartOf = [ "swww.service" ];
        After = [ "swww.service" ];
      };
      Install.WantedBy = [ config.wayland.systemd.target ];
      Service = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs [
          "${lib.getExe self.pkgs.swww-util}"
          "random" cfg.directory
        ];
      };
    };
  };
}
