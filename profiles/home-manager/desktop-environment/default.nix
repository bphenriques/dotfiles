{ config, lib, pkgs, osConfig, ... }:
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./dunst.nix             # Notifications
    ./hypridle.nix          # Idle behaviour
    ./hyprlock.nix          # Lock screen
    ./niri.nix              # Window Manager
  ];

  # TODO: replace with services.awww.enable once home-manager adds the module (swww was renamed to awww).
  home.packages = [ pkgs.awww ];
  systemd.user.services.awww = {
    Install.WantedBy = [ config.wayland.systemd.target ];
    Unit = {
      ConditionEnvironment = "WAYLAND_DISPLAY";
      Description = "awww-daemon";
      After = [ config.wayland.systemd.target ];
      PartOf = [ config.wayland.systemd.target ];
    };
    Service = {
      ExecStart = "${lib.getExe' pkgs.awww "awww-daemon"}";
      Environment = [ "PATH=$PATH:${lib.makeBinPath [ pkgs.awww ]}" ];
      Restart = "always";
      RestartSec = 10;
    };
  };

  custom.programs.screenshot.enable = true;
  custom.programs.screen-recorder.enable = true;
  custom.programs.session.enable = true;
  custom.programs.volume-osd.enable = true;
  custom.programs.brightness-osd.enable = true;
  custom.programs.file-explorer.browser = "${lib.getExe' pkgs.foot "footclient"} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";
  custom.services.upower-notify = { inherit (osConfig.services.upower) enable percentageLow percentageCritical; };
}
