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

  custom.programs = {
    screenshot.enable = true;
    screen-recorder.enable = true;
    session.enable = true;
    volume-osd.enable = true;
    brightness-osd.enable = true;
    file-explorer.browser = "${lib.getExe pkgs.ghostty} +new-window --title=yazi-tui -e ${lib.getExe config.programs.yazi.package}";
  };

  custom.services = {
    upower-notify = { inherit (osConfig.services.upower) enable percentageLow percentageCritical; };
  };

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.xwayland-satellite       # Needed in PATH for niri's built-in xwayland support
  ];
}
