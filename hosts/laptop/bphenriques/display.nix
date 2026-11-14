# Screens, brightness and display layouts; see ../README.md#display.
{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  brightnessctl = "${lib.getExe pkgs.brightnessctl} --class=backlight";
  brightnessDown = "${brightnessctl} set 5%-";
  brightnessUp = "${brightnessctl} set 5%+";
  brightness = [ brightnessDown brightnessUp ];

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
    { id = "laptop";   label = "Laptop only";   icon = self.lib.glyphs."md-laptop"; }
    { id = "external"; label = "External only"; icon = self.lib.glyphs."md-monitor"; }
    { id = "extend";   label = "Extend";        icon = self.lib.glyphs."md-monitor_multiple"; }
  ];

  menuKey = "d";

  displayctl = lib.getExe (self.packages.displayctl.override { inherit panel; });

  # displayctl's status as the displayLayout plugin's state line.
  layoutState = pkgs.writeShellApplication {
    name = "display-layout-state";
    runtimeInputs = [ pkgs.jq ];
    text = ''
      ${displayctl} status | jq -cRs \
        --argjson layouts ${lib.escapeShellArg (builtins.toJSON layouts)} \
        --arg fallback ${lib.escapeShellArg self.lib.glyphs."md-monitor"} \
        -f ${./display-layouts.jq}
    '';
  };
in
{
  wayland.windowManager.niri.settings._children = map (screen: { output = { _args = [ screen.name ]; inherit (screen) mode scale; }; }) screens;

  programs.quickkey.settings = {
    brightness = {
      backlight = "${brightnessctl} --machine-readable info | ${lib.getExe pkgs.gawk} -F, -v OFS='\\t' '{ print \"/sys/class/backlight/\" $1 \"/brightness\", $5 }'";
      adjust = brightness;
      inherit panel;
    };
    plugins.displayLayout = {
      source = "command";
      settings = {
        read = lib.getExe layoutState;
        select = "${displayctl} layout";
      };
    };
    menu.entries = lib.mkOrder 500 [
      { key = menuKey; desc = "Display"; status = "brightness"; list = "displayLayout"; submenu = [
        { key = [ "Down" "Up" ]; desc = "Brightness"; icon = self.lib.glyphs."md-brightness_6"; cmd = brightness; keepOpen = true; requires.enabled = [ panel ]; }
        { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = lib.getExe pkgs.wdisplays; }
      ]; }
    ];
  };

  my.programs.niri.bindings = {
    "Mod+P repeat=false" = ''spawn-sh "${config.programs.quickkey.exec.ipc} menu toggleEntry ${menuKey}"'';
    "XF86MonBrightnessUp allow-when-locked=true" = ''spawn-sh "${brightnessUp}"'';
    "XF86MonBrightnessDown allow-when-locked=true" = ''spawn-sh "${brightnessDown}"'';
  };

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
