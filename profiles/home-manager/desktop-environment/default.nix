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
    niri-keyboard-layout.enable = true;
    file-explorer.browser = config.custom.programs.terminal.execApp { title = "yazi-tui"; cmd = lib.getExe config.programs.yazi.package; };
    satty = { enable = true; directory = config.custom.programs.screenshot.directory; };
    status-glance.enable = true;
  };

  custom.services = {
    upower-notify = { inherit (osConfig.services.upower) enable percentageLow percentageCritical; };
  };

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.xwayland-satellite       # Needed in PATH for niri's built-in xwayland support
  ];
}
