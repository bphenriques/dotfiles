{ config, lib, pkgs, self, osConfig, ... }:
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./mako.nix              # Notifications
    ./swayidle.nix          # Idle behaviour
    ./niri.nix              # Window Manager
  ];

  custom.programs.screenshot.enable = true;
  custom.programs.screen-recorder.enable = true;
  custom.programs.shortcuts.files.browser = "${lib.getExe' pkgs.foot "footclient"} --title=yazi-tui ${lib.getExe pkgs.yazi}";

  home.packages = [
    pkgs.wdisplays

    # Management
    pkgs.pavucontrol
    self.pkgs.volume-osd
    self.pkgs.brightness-osd
    self.pkgs.powerprofilesctl-notify
    self.pkgs.niri-keyboard-layout
  ];
}