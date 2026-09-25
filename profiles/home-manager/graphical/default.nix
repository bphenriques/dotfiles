{ config, lib, osConfig, ... }:
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./dunst.nix             # Notifications
    ./hypridle.nix          # Idle behaviour
    ./hyprlock.nix          # Lock screen
    ./niri.nix              # Window Manager
  ];

  my.programs = {
    screenshot.enable = true;
    screen-recorder.enable = true;
    session.enable = true;
    volume-osd.enable = true;
    brightness-osd.enable = true;
    niri-keyboard-layout.enable = true;
    file-explorer.browser = config.my.programs.terminal.execApp { title = "yazi-tui"; cmd = lib.getExe config.programs.yazi.package; };
    status-glance.enable = true;
  };

  programs.satty = {
    enable = true;
    settings.general = {
      fullscreen = true;
      early-exit = true;
      disable-notifications = false;
      initial-tool = "brush";
      copy-command = "wl-copy";
      save-after-copy = false;
      output-filename = with config.my.programs.screenshot; "${directory}/${format}";
    };
  };

  my.services = {
    upower-notify = { inherit (osConfig.services.upower) enable percentageLow percentageCritical; };
  };
}
