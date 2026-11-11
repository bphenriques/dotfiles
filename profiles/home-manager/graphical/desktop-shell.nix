{ config, lib, pkgs, osConfig, self, ... }:
let
  cfg = config.my.programs.desktop-shell;
  colors = config.lib.stylix.colors.withHashtag;
  nmcli = lib.getExe' pkgs.networkmanager "nmcli";
  niri = lib.getExe pkgs.niri;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  my.programs.desktop-shell = {
    enable = true;

    settings = {
      theme = {
        background = colors.base00;
        foreground = colors.base05;
        primary = colors.base0D;
        alert = colors.base08;
        warning = colors.base0A;
        monospaceFont = config.stylix.fonts.monospace.name;
        fontSize = config.stylix.fonts.sizes.popups;
      };
      battery = {
        low = osConfig.services.upower.percentageLow;
        critical = osConfig.services.upower.percentageCritical;
      };
      keyboard.labels = lib.map (layout: layout.label) config.my.programs.niri.keyboardLayouts;
      commands = {
        brightness = "${lib.getExe pkgs.brightnessctl} --machine-readable --class=backlight info | ${lib.getExe pkgs.gawk} -F, -v OFS='\\t' '{ print \"/sys/class/backlight/\" $1 \"/brightness\", $5 }'";
        outputs = "${niri} msg --json outputs";
        # exec, so stopping it closes the inhibitor's stdin rather than orphaning it with the lock held.
        keepAwake = "exec ${lib.getExe' pkgs.systemd "systemd-inhibit"} --what=idle --who=desktop-shell --why=\"Keep awake\" cat";
      };
    };

    menu.entries = lib.mkMerge [
      (lib.mkOrder 300 [
        { key = "n"; desc = "Network"; width = 640; status = "network"; list = "wifi"; submenu = [
          { key = "p";     desc = "Power"; icon = self.lib.glyphs."md-toggle_switch";      activeWhen = "wifi"; cmd = ''${nmcli} radio wifi "$([ "$(${nmcli} radio wifi)" = enabled ] && echo off || echo on)"''; keepOpen = true; }
          { key = "c";     desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = config.my.programs.terminal.execApp { title = "nmtui-tui"; cmd = lib.getExe' pkgs.networkmanager "nmtui"; }; }
        ]; }
      ])
      (lib.mkOrder 800 [
        { key = "t"; desc = "Tray"; width = 640; status = "tray"; list = "tray"; }
      ])
    ];
  };

  my.programs.niri = {
    bindings = {
      "Mod+Ctrl+Space repeat=false" = ''spawn-sh "${cfg.exec.ipc} menu toggle"'';
      # A spawn, not the switch-layout action: only spawns run while locked.
      "Mod+K allow-when-locked=true" = ''spawn-sh "${niri} msg action switch-layout next"'';
      "Mod+I" = ''spawn-sh "${cfg.exec.ipc} idle toggle"'';
    };
    layerRules = {
      screenCapture.block = [ ''namespace="^recording-indicator$"'' ];
    };
  };
}
