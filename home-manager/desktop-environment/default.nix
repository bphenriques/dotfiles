{ config, lib, pkgs, self, osConfig, ... }:
let
  terminal = lib.getExe' config.programs.foot.package "footclient";
  screen-lock = ''${lib.getExe pkgs.niri} msg action do-screen-transition --delay-ms 750 && ${lib.getExe pkgs.hyprlock}'';
  application-launcher = lib.getExe pkgs.fuzzel;
  system-monitor = ''${terminal} --title=btop-tui ${lib.getExe pkgs.btop}'';
  filebrowser = "${terminal} --title=yazi-tui ${lib.getExe config.programs.yazi.package}";
in
{
  imports = [
    ./qt.nix        # Setup theming for some set of apps
    ./gtk.nix       # Setup theming for some set of apps
    ./niri.nix      # Window Manager
    ./waybar        # Status bar
    ./mako.nix      # Notification Daemon
    ./fuzzel.nix    # Application Launcher
    ./rofi          # Application Launcher
    ./swayidle.nix  # Locks/suspends the computer when idle
    ./hyprlock.nix  # Lock screend
    ./osd.nix       # On Screen Display
    ./swww.nix      # Backend to manage wallpapers
  ];

  custom.desktop-environment = {
    inherit application-launcher system-monitor screen-lock terminal;

    window-switcher = lib.getExe self.pkgs.niri-window-dmenu;
    file-browser = "${filebrowser} ~";

    session-menu = lib.getExe (self.lib.builders.writeDmenuScript pkgs {
      name = "session-dmenu";
      entries = [
        { label = "    Shutdown";           exec = "systemctl poweroff"; }
        { label = "    Reboot";             exec = "systemctl reboot"; }
        { label = "    Lock";               exec = screen-lock; }
        { label = "󰤄    Suspend";            exec = "systemctl suspend"; }
      ] ++ lib.optionals (osConfig.custom.boot.grub.windowsEfiDevice != "") [
        { label = "    Reboot to Windows";  exec = lib.getExe osConfig.custom.boot.grub.windowsRebootPackage; }
      ] ++ [
        { label = "󰞱    System Monitor";     exec = system-monitor; }
      ];
    });
    screenshot-menu = lib.getExe (self.lib.builders.writeDmenuScript pkgs {
      name = "screenshot-dmenu";
      entries = [
        { label = "    Shutdown";  exec = "systemctl poweroff"; }
      ];
    });

    # FIXME
    emoji-picker = ''BEMOJI_ECHO_NEWLINE=false BEMOJI_PICKER_CMD=${application-launcher} -d ${lib.getExe pkgs.bemoji}'';
  };
}