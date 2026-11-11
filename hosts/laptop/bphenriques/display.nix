# Screens and what switches them: niri holds each screen's settings, the Display menu picks a
# layout, and a reactor restores the one last picked for what is plugged in. Anything finer
# (a subset of screens, positions, rotation) is for Configure or the niri config.
{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  brightnessctl = "${lib.getExe pkgs.brightnessctl} --class=backlight";
  brightness = [ "${brightnessctl} set 5%-" "${brightnessctl} set 5%+" ];

  # The panel. niri only knows it by its connector name; the externals below are named by make,
  # model and serial so they keep their settings when they move to another port.
  panel = "eDP-1";

  # Modes are the exact strings `niri msg --json outputs` reports, refresh in mHz: the highest the
  # screen accepts at the resolution wanted. A mode niri cannot set it ignores, and picks one itself.
  screens = [
    { name = panel; mode = "2880x1800@120.001"; scale = 1.75; }
    { name = "Dell Inc. DELL S2721DGF 4P11R83"; mode = "2560x1440@143.912"; scale = 1.0; }
    { name = "LG Electronics LG TV SSCR2 0x01010101"; mode = "2560x1440@119.998"; scale = 1.0; }
  ];

  # Windows' Win+P without Duplicate: niri cannot mirror yet.
  layouts = [
    { key = "l"; desc = "Laptop only";   layout = "laptop"; }
    { key = "e"; desc = "External only"; layout = "external"; }
    { key = "x"; desc = "Extend";        layout = "extend"; }
  ];

  menuKey = "d";

  # md-laptop, md-monitor, md-monitor_multiple: each layout's icon, also on the Display row.
  glyphs = { laptop = self.lib.glyphs."md-laptop"; external = self.lib.glyphs."md-monitor"; extend = self.lib.glyphs."md-monitor_multiple"; };

  displayctl = lib.getExe (self.packages.displayctl.override { inherit panel; });
in
{
  wayland.windowManager.niri.settings._children = map (screen: { output = { _args = [ screen.name ]; inherit (screen) mode scale; }; }) screens;

  my.programs.desktop-shell = {
    settings = {
      osd.adjust.brightness = brightness;
      commands.displayLayout = "${displayctl} status";
      display.panel = panel;
      glyphs.display = glyphs;
      display.layouts = lib.listToAttrs (map (layout: lib.nameValuePair layout.layout layout.desc) layouts);
    };
    menu.entries = lib.mkOrder 500 [
      { key = menuKey; desc = "Display"; status = "display"; submenu = [
        { key = [ "Down" "Up" ]; desc = "Brightness"; icon = self.lib.glyphs."md-brightness_6"; cmd = brightness; keepOpen = true; requires.enabled = [ panel ]; }
        { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = lib.getExe pkgs.wdisplays; }
      ] ++ map (layout: layout // { cmd = "${displayctl} layout ${layout.layout}"; icon = glyphs.${layout.layout}; }) layouts; }
    ];
  };

  my.programs.niri.bindings."Mod+P repeat=false" = ''spawn-sh "${config.my.programs.desktop-shell.exec.ipc} menu toggleEntry ${menuKey}"'';

  systemd.user.services.display-policy = {
    Unit = {
      Description = "Restore the display layout last picked for the screens plugged in";
      PartOf = [ config.wayland.systemd.target ];
      After = [ config.wayland.systemd.target ];
      X-Restart-Triggers = [ displayctl ];
    };
    Service = {
      ExecStart = "${displayctl} follow";
      Restart = "always";
      RestartSec = 5;
    };
    Install.WantedBy = [ config.wayland.systemd.target ];
  };
}
