{ config, lib, pkgs, osConfig, ... }:
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./dunst.nix             # Notifications
    ./hypridle.nix          # Idle behaviour
    ./hyprlock.nix          # Lock screen
    ./niri.nix              # Window Manager
    ./wl-kbptr.nix          # Mouse simulation
  ];

  services.swww.enable = true;

  custom.programs.screenshot.enable = true;
  custom.programs.screen-recorder.enable = true;
  custom.programs.session.enable = true;
  custom.programs.volume-osd.enable = true;
  custom.programs.brightness-osd.enable = true;
  custom.programs.file-explorer.browser = "${lib.getExe' pkgs.foot "footclient"} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";
  custom.services.upower-notify = { inherit (osConfig.services.upower) enable percentageLow percentageCritical; };
}
