{ config, lib, pkgs, self, osConfig, ... }:
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./mako.nix              # Notifications
    ./hypridle.nix          # Idle behaviour
    ./hyprlock.nix          # Lock screen
    ./niri.nix              # Window Manager
  ];

  custom.programs.screenshot.enable = true;
  custom.programs.screen-recorder.enable = true;
  custom.programs.powerprofilesctl.enable = true;
  custom.programs.session.enable = true;
  custom.programs.volume-osd.enable = true;
  custom.programs.file-explorer.browser = "${lib.getExe' pkgs.foot "footclient"} --title=yazi-tui ${lib.getExe pkgs.yazi}";

  custom.services.swww.enable = true;
  custom.services.upower-notify.enable = osConfig.services.upower.enable or false;
}