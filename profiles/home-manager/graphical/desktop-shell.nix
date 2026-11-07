{ config, lib, pkgs, osConfig, ... }:
let
  cfg = config.my.programs.desktop-shell;
  colors = config.lib.stylix.colors.withHashtag;
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
    };

    menu.entries = lib.mkMerge [
      (lib.mkOrder 1000 [
        { key = "n"; desc = "Network Manager"; status = "network"; cmd = config.my.programs.terminal.execApp { title = "nmtui-tui"; cmd = lib.getExe' pkgs.networkmanager "nmtui"; }; }
      ])
      (lib.mkOrder 1100 [
        { key = "t"; desc = "Tray"; status = "tray"; list = "tray"; }
      ])
    ];
  };

  my.programs.niri = {
    bindings."Mod+Ctrl+Space" = ''spawn-sh "${cfg.exec.menu}"'';
    layerRules = {
      screenCapture.block = [ ''namespace="^recording-indicator$"'' ];
      base = [
        ''
          layer-rule {
            match namespace="^menu$"
            shadow {
              on
            }

            geometry-corner-radius ${toString cfg.settings.theme.radius}

            background-effect {
              blur true
            }
          }
        ''
      ];
    };
  };
}
